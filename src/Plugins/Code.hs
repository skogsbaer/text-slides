{-# LANGUAGE LambdaCase #-}
{-

A plugin call such as

~~~python
BODY
~~~

simply places to same content in the markdown file. The p_forAllCalls action (invoked when
generating the main output document) then writes to code snippets into the appropriate
files.
-}
module Plugins.Code (codePlugins, LangConfig (..), mkLangConfig) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Development.Shake
import Logging
import System.FilePath
import Types
import Utils
import qualified Data.Set as Set
import qualified Data.Bifunctor
import Safe
import System.Directory

data CodeMode = CodeModeShow | CodeModeHide | CodeModeShowOnly
  deriving (Eq, Show)

data LineNumberMode = LineNumbersOff | LineNumbersOn | LineNumbersAuto
  deriving (Eq, Show)

data FirstLine = FirstLineImplicit | FirstLineExplicit Int | FirstLineContinue
  deriving (Eq, Show)

data CodePlace = AtStart | Here | AtEnd | Tagged Tag
  deriving (Eq, Show)

data Tag =
  Tag { t_kind :: T.Text, t_id :: T.Text }
  deriving (Eq, Show, Ord)

data CodeArgs = CodeArgs
  { ca_file :: Maybe FilePath,
    ca_mode :: CodeMode,
    ca_lineNumberMode :: LineNumberMode,
    ca_firstLine :: FirstLine,
    ca_place :: CodePlace,
    ca_comment :: Bool,
    ca_prepend :: Maybe T.Text
  }

parseArgs :: PluginCall -> [T.Text] -> Fail CodeArgs
parseArgs call extraArgs = do
  file <- getOptionalStringValue loc "file" m
  modeStr <- getOptionalEnumValue loc "mode" ["show", "hide", "showOnly"] m
  lineNumStr <- getOptionalEnumValue loc "lineNumbers" ["on", "off", "auto"] m
  placeStr <- getOptionalEnumValue loc "place" ["atStart", "here", "atEnd"] m
  comment <- fromMaybe False <$> getOptionalBoolValue loc "comment" m
  prepend <- getOptionalStringValue loc "prepend" m
  mode <-
    case modeStr of
      Just "show" -> return CodeModeShow
      Just "hide" -> return CodeModeHide
      Just "showOnly" -> return CodeModeShowOnly
      Nothing -> return CodeModeShow
      Just x -> error $ "uncovered case: " ++ show x
  lineNum <-
    case lineNumStr of
      Just "on" -> return LineNumbersOn
      Just "off" -> return LineNumbersOff
      Just "auto" -> return LineNumbersAuto
      Nothing -> return LineNumbersAuto
      Just x -> error $ "uncovered case: " ++ show x
  place <-
    case placeStr of
      Just "atStart" -> return AtStart
      Just "here" -> return Here
      Just "atEnd" -> return AtEnd
      Nothing -> return Here
      Just x -> error $ "uncovered case: " ++ show x
  firstLineM <- getOptionalValue loc "firstLine" m "Int or \"continue\"" $ \case
      ArgInt i -> Just $ FirstLineExplicit i
      ArgString "continue" -> Just FirstLineContinue
      _ -> Nothing
  let firstLine = fromMaybe FirstLineImplicit firstLineM
  checkForSpuriousArgs loc m
      (["file", "mode", "lineNumbers", "firstLine", "place", "comment", "prepend"] ++ extraArgs)
  return $
    CodeArgs
      { ca_file = fmap T.unpack file,
        ca_mode = mode,
        ca_lineNumberMode = lineNum,
        ca_firstLine = firstLine,
        ca_place = place,
        ca_comment = comment,
        ca_prepend = prepend
      }
  where
    loc = pc_location call
    m = pc_args call

data CodeState = CodeState
  {cs_nextLineNumber :: Int}

initialCodeState :: CodeState
initialCodeState = CodeState 1

mkCodePlugin :: PluginName -> LangConfig -> PluginConfig CodeState Action
mkCodePlugin name cfg =
  PluginConfig
    { p_name = name,
      p_kind = PluginWithBody,
      p_rules = pluginRules,
      p_init = return initialCodeState,
      p_expand = runPlugin cfg,
      p_forAllCalls = processAllCalls cfg
    }

pluginRules :: BuildConfig -> BuildArgs -> Rules ()
pluginRules _cfg _args = return ()

runPlugin ::
  LangConfig ->
  BuildConfig ->
  BuildArgs ->
  CodeState ->
  PluginCall ->
  ExceptT T.Text Action (T.Text, CodeState)
runPlugin langCfg _cfg _buildArgs state call = do
  args <- exceptInM $ parseArgs call (lc_extraArgs langCfg)
  let showLineNumbers =
        case ca_lineNumberMode args of
          LineNumbersOff -> False
          LineNumbersOn -> True
          LineNumbersAuto -> length (T.lines (pc_body call)) > 5
      firstLine =
        case ca_firstLine args of
          FirstLineImplicit -> 1
          FirstLineExplicit i -> i
          FirstLineContinue -> cs_nextLineNumber state
      body = pc_body call
      nextState =
        CodeState
          { cs_nextLineNumber =
              if ca_mode args == CodeModeHide
                then cs_nextLineNumber state
                else if showLineNumbers then firstLine + length (T.lines body) else 1
          }
      header =
        "~~~{"
          <> "."
          <> unPluginName (pc_pluginName call)
          <> ( if showLineNumbers
                 then " .numberLines startFrom=\"" <> showText firstLine <> "\""
                 else ""
             )
          <> "}"
      code = header <> "\n" <> body <> "\n~~~"
      result =
        case ca_mode args of
          CodeModeShow -> code
          CodeModeShowOnly -> code
          CodeModeHide -> ""
  return (result, nextState)

data CollectedCode = CollectedCode
  { cc_code :: T.Text,
    cc_sectionName :: Maybe T.Text
  }
  deriving (Eq, Show)

data CollectedCodeFile = CollectedCodeFile
  { ccf_atStart :: [CollectedCode], -- reversed
    ccf_here :: [CollectedCode], -- reversed
    ccf_atEnd :: [CollectedCode], -- reversed
    ccf_tagged :: [(Tag, CollectedCode)] -- reversed
  }
  deriving (Show)

emptyCollectedCodeFile :: CollectedCodeFile
emptyCollectedCodeFile = CollectedCodeFile [] [] [] []

mkCollectedCodeFile :: CollectedCode -> CodePlace -> CollectedCodeFile
mkCollectedCodeFile cc place =
  case place of
    AtStart -> emptyCollectedCodeFile {ccf_atStart = [cc]}
    Here -> emptyCollectedCodeFile {ccf_here = [cc]}
    AtEnd -> emptyCollectedCodeFile {ccf_atEnd = [cc]}
    Tagged t -> emptyCollectedCodeFile {ccf_tagged = [(t, cc)]}

appendCollectedCode :: CollectedCode -> CodePlace -> CollectedCodeFile -> CollectedCodeFile
appendCollectedCode cc place file =
  case place of
    AtStart -> file {ccf_atStart = cc : ccf_atStart file}
    Here -> file {ccf_here = cc : ccf_here file}
    AtEnd -> file {ccf_atEnd = cc : ccf_atEnd file}
    Tagged tag -> file {ccf_tagged = (tag, cc) : (ccf_tagged file) }

processAllCalls ::
  LangConfig -> BuildConfig -> BuildArgs -> [PluginCall] -> ExceptT T.Text Action ()
processAllCalls langCfg cfg buildArgs calls = do
  let allPlugins = Set.fromList (map pc_pluginName  calls)
  forM_ allPlugins $ \pluginName -> do
    let dir = pluginDir cfg pluginName
    liftIO $ removeDirectoryRecursive dir
  codeMap <- exceptInM $ foldM collectCode M.empty calls
  time <- liftIO getCurrentTime
  let header =
        cmt
          ( "Automatically extracted from " <> T.pack (ba_inputFile buildArgs)
              <> " on "
              <> showText time
          )
  forM_ (M.toList codeMap) $ \(file, ccf) -> do
    let code =
          mkCode (ccf_atStart ccf) <>
          mkCode (ccf_here ccf) <>
          lc_mkCodeForTags langCfg (mkCodeForTagged (ccf_tagged ccf)) <>
          mkCode (ccf_atEnd ccf)
    lift $ note ("Generating " ++ file)
    lift $ myWriteFile file (header <> code)
  where
    cmt = lineComment langCfg
    mkCodeForTagged :: [(Tag, CollectedCode)] -> [(Tag, T.Text)]
    mkCodeForTagged = map (Data.Bifunctor.second cc_code)
    mkCode :: [ CollectedCode] -> T.Text
    mkCode revCode =
      let groupedBySectionName =
            L.groupBy (\x y -> cc_sectionName x == cc_sectionName y) (reverse revCode)
          code =
            T.unlines $
              flip concatMap groupedBySectionName $ \ccs ->
                case ccs of
                  [] -> []
                  (cc : _) ->
                    ( case cc_sectionName cc of
                        Just x ->
                          let line = T.replicate (T.length x) "-"
                           in ["", "", cmt line, cmt x, cmt line]
                        Nothing -> []
                    )
                      ++ map cc_code ccs
       in code
    collectCode ::
      M.Map FilePath CollectedCodeFile ->
      PluginCall ->
      Fail (M.Map FilePath CollectedCodeFile)
    collectCode m call = do
      (file, mCode) <- codeFromCall call
      case mCode of
        Nothing -> return m
        Just (place, code) ->
          let cc = CollectedCode code (pc_sectionName call)
           in case M.lookup file m of
                Just ccf -> return $ M.insert file (appendCollectedCode cc place ccf) m
                Nothing -> return $ M.insert file (mkCollectedCodeFile cc place) m
    codeFromCall :: PluginCall -> Fail (FilePath, Maybe (CodePlace, T.Text))
    codeFromCall call = do
      args <- parseArgs call (lc_extraArgs langCfg)
      let baseFile =
            case ca_file args of
              Nothing ->
                lc_makeDefaultFilename langCfg $
                replaceExtension (ba_inputFile buildArgs) (lc_fileExt langCfg)
              Just f -> f
          file = pluginDir cfg (pc_pluginName call) </> baseFile
          body =
            maybe "" (\t -> t <> "\n") (ca_prepend args) <>
            (comment (ca_comment args) $ T.stripEnd (pc_body call))
          code = "\n" <> cmt ("[" <> unLocation (pc_location call) <> "]") <> "\n" <> body
      mCode <-
        case ca_mode args of
          CodeModeShowOnly -> return Nothing
          _ -> do
            mTag <- lc_tagForCall langCfg call
            case mTag of
              Just tag -> return $ Just (Tagged tag, code)
              Nothing -> return $ Just (ca_place args, code)
      return (file, mCode)
    comment False t = t
    comment True t =
      T.unlines $
        map (lineComment langCfg) $
          T.lines t

data LangConfig = LangConfig
  { lc_fileExt :: String,
    lc_commentStart :: T.Text,
    lc_commentEnd :: Maybe T.Text,
    lc_makeDefaultFilename :: FilePath -> FilePath,
    lc_extraArgs :: [T.Text],
    lc_tagForCall :: PluginCall -> Fail (Maybe Tag),
    lc_mkCodeForTags :: [(Tag, T.Text)] -> T.Text
  }

mkLangConfig :: String -> T.Text -> Maybe T.Text -> LangConfig
mkLangConfig ext commentStart commentEnd =
  LangConfig
  { lc_fileExt = ext,
    lc_commentStart = commentStart,
    lc_commentEnd = commentEnd,
    lc_makeDefaultFilename = id,
    lc_extraArgs = [],
    lc_tagForCall = const (Right Nothing),
    lc_mkCodeForTags = const ""
  }

-- The input list may contain several entries for the same tag, the output list
-- contains at most one entry for the same tag (the content is concatenated).
groupByTag :: [(Tag, T.Text)] -> [(Tag, T.Text)]
groupByTag l =
  let m = M.fromListWith (\old new -> old <> "\n" <> new) l
  in loop Set.empty m l []
  where
    loop :: Set.Set Tag -> M.Map Tag T.Text -> [(Tag, T.Text)] -> [(Tag, T.Text)] -> [(Tag, T.Text)]
    loop _ _ [] acc = reverse acc
    loop handled m ((tag, _) : rest) acc =
      if tag `Set.member` handled
        then loop handled m rest acc
        else loop (Set.insert tag handled) m rest
               ((tag, (fromJustNote ("expected tag " ++ show tag ++ " in map") $ M.lookup tag m))
                : acc)

javaLangConfig :: LangConfig
javaLangConfig = (mkLangConfig ".java" "// " Nothing)
  { lc_makeDefaultFilename = \fp ->
      let replace c =
            case c of
              '-' -> '_'
              _ -> c
      in takeDirectory fp </> "Class_" ++ map replace (takeFileName fp),
    lc_extraArgs = ["method", "body"],
    lc_tagForCall = \call -> do
      let loc = pc_location call
          m = pc_args call
      method <- fromMaybe False <$> getOptionalBoolValue loc "method" m
      body <- case M.lookup "body" m of
        Nothing -> pure Nothing
        Just (ArgString t) -> pure $ Just (Tag "body"  t)
        Just (ArgInt i) -> pure $ Just (Tag "body" (showText i))
        Just (ArgBool True) -> pure $ Just (Tag "body" (hash call))
        Just (ArgBool False) -> pure Nothing
      case (method, body) of
        (False, _) -> Right body
        (True, Nothing) -> Right (Just (methodTag (hash call)))
        (True, Just _) -> Left (unLocation loc <> ": cannot set method and body"),
    lc_mkCodeForTags = \m' ->
      let m = groupByTag m'
          methods = map snd $ filter (\(t, _) -> t_kind t == "method") m
          bodies = map (Data.Bifunctor.first t_id) $ filter (\(t, _) -> t_kind t == "body") m
          methodsForBodies = flip map bodies $ \(id, code) ->
            "public static void __body_" <> id <> "() throws Exception {\n" <> code <> "\n}"
          allMethods = methods ++ methodsForBodies
      in if null allMethods
            then ""
            else let code = T.concat (L.intersperse "\n\n" allMethods)
                 in "class __CodeContainer {\n" <> code <> "\n}"
  }
  where
    methodTag = Tag "method"
    hash c = unHash (md5OfText (unLocation (pc_location c)))

lineComment :: LangConfig -> T.Text -> T.Text
lineComment cfg t = lc_commentStart cfg <> t <> fromMaybe "" (lc_commentEnd cfg)

codePlugins :: [(T.Text, LangConfig)] -> [AnyPluginConfig Action]
codePlugins moreLangs =
  flip map (filter (\(k, _) -> not (k `Set.member` custom)) languages ++ moreLangs) $ \(name, langCfg) ->
  AnyPluginConfig $ mkCodePlugin (PluginName name) langCfg
  where
    custom = Set.fromList (map fst moreLangs)

languages :: [(T.Text, LangConfig)]
languages =
  [ ("bash", mkLangConfig ".sh" "# " Nothing),
    ("c", mkLangConfig ".c" "// " Nothing),
    ("cs", mkLangConfig ".cs" "// " Nothing),
    ("css", mkLangConfig ".css" "// " Nothing),
    ("clojure", mkLangConfig ".clj" ";; " Nothing),
    ("erlang", mkLangConfig ".erl" "% " Nothing),
    ("fsharp", mkLangConfig ".fs" "// " Nothing),
    ("html", mkLangConfig ".html" "<!-- " (Just " -->")),
    ("haskell", mkLangConfig ".hs" "-- " Nothing),
    ("json", mkLangConfig ".json" "// " Nothing),
    ("java", javaLangConfig),
    ("javascript", mkLangConfig ".js" "// " Nothing),
    ("typescript", mkLangConfig ".ts" "// " Nothing),
    ("ocaml", mkLangConfig ".ml" "(* " (Just " *)")),
    ("objectivec", mkLangConfig ".m" "// " Nothing),
    ("python", mkLangConfig ".py" "# " Nothing),
    ("rust", mkLangConfig ".rs" "// " Nothing),
    ("scheme", mkLangConfig ".sc" ";; " Nothing),
    ("xml", mkLangConfig ".xml" "<!-- " (Just " -->"))
  ]
