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
module Plugins.Code (codePlugins, LangConfig (..), mkLangConfig, ExternalLangConfig(..)) where

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
import Control.Exception

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
    ca_prepend :: Maybe T.Text,
    ca_rewrite :: Rewrite
  }

parseArgs :: PluginCall -> [T.Text] -> Fail CodeArgs
parseArgs call extraArgs = do
  file <- getOptionalStringValue loc "file" m
  modeStr <- getOptionalEnumValue loc "mode" ["show", "hide", "showOnly"] m
  lineNumStr <- getOptionalEnumValue loc "lineNumbers" ["on", "off", "auto"] m
  rewriteBool <- getOptionalBoolValue  loc "rewrite" m
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
  rewrite <-
    case rewriteBool of
      Just True -> return DoRewrite
      Just False -> return NoRewrite
      Nothing -> return NoRewrite
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
      (["file", "mode", "lineNumbers", "firstLine", "place", "comment", "prepend", "rewrite"]
       ++ extraArgs)
  return $
    CodeArgs
      { ca_file = fmap T.unpack file,
        ca_mode = mode,
        ca_lineNumberMode = lineNum,
        ca_firstLine = firstLine,
        ca_place = place,
        ca_comment = comment,
        ca_prepend = prepend,
        ca_rewrite = rewrite
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
      body = extractCode langCfg CodeExctractPresentation (Code (ca_rewrite args) (pc_body call))
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

data Rewrite
  = DoRewrite
  | NoRewrite
  deriving (Eq, Show)

-- The text wrapped in the Code newtype may contain directives
-- such as `# ~~~hide`.
data Code =
  Code
  { c_rewrite :: Rewrite
  , c_payload :: T.Text
  }
  deriving (Eq, Show)

data CodeExtract = CodeExtractFile | CodeExctractPresentation

extractCode :: LangConfig -> CodeExtract -> Code -> T.Text
extractCode lcfg mode (Code doRewrite t) =
  let lines = T.lines t
      newLines = loop True lines []
  in T.unlines newLines
  where
    loop _ [] acc = reverse acc
    loop show (l:ls) acc =
      case parseShowHide l of
        Nothing ->
          loop show ls (if show then (rewriteCodeLine lcfg mode doRewrite l : acc) else acc)
        Just parsedShow ->
          let newShow =
                case mode of
                  CodeExtractFile -> True
                  CodeExctractPresentation -> parsedShow
          in loop newShow ls acc
    parseShowHide (T.stripEnd -> t) = do
      t <- T.stripPrefix (lc_commentStart lcfg) t
      t <- T.stripSuffix (fromMaybe "" (lc_commentEnd lcfg)) t
      case T.strip t of
        "~~~hide" -> pure False
        "~~~show" -> pure True
        _ -> Nothing

rewriteCodeLine :: LangConfig -> CodeExtract -> Rewrite -> T.Text -> T.Text
rewriteCodeLine lcfg mode doRewrite line =
  case (mode, doRewrite) of
    (CodeExctractPresentation, _) -> line
    (CodeExtractFile, NoRewrite) -> line
    (CodeExtractFile, DoRewrite) -> lc_rewriteLine lcfg line

data CollectedCode = CollectedCode
  { cc_code :: Code,
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
    liftIO $ removeAllFilesInDirectory dir `catch` (\(_::IOError) -> pure ())
  codeMap <- exceptInM $ foldM collectCode M.empty calls
  time <- liftIO getCurrentTime
  let header =
        cmt
          ( "Automatically extracted from " <> T.pack (ba_inputFile buildArgs)
              <> " on "
              <> showText time <> "\n\n"
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
    getCode = extractCode langCfg CodeExtractFile . cc_code
    mkCodeForTagged :: [(Tag, CollectedCode)] -> [(Tag, T.Text)]
    mkCodeForTagged = map (Data.Bifunctor.second getCode)
    mkCode :: [CollectedCode] -> T.Text
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
                      ++ map getCode ccs
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
    codeFromCall :: PluginCall -> Fail (FilePath, Maybe (CodePlace, Code))
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
          code =
            Code (ca_rewrite args) $
              "\n" <> cmt ("[" <> unLocation (pc_location call) <> "]") <> "\n" <> body
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
    lc_mkCodeForTags :: [(Tag, T.Text)] -> T.Text,
    lc_rewriteLine :: T.Text -> T.Text
  }

data ExternalLangConfig = ExternalLangConfig
  { elc_name :: T.Text,
    elc_fileExt :: String,
    elc_commentStart :: T.Text,
    elc_commentEnd :: Maybe T.Text,
    elc_syntaxFile :: Maybe FilePath
  }
  deriving (Show, Eq)

mkLangConfig :: String -> T.Text -> Maybe T.Text -> LangConfig
mkLangConfig ext commentStart commentEnd =
  LangConfig
  { lc_fileExt = ext,
    lc_commentStart = commentStart,
    lc_commentEnd = commentEnd,
    lc_makeDefaultFilename = id,
    lc_extraArgs = [],
    lc_tagForCall = const (Right Nothing),
    lc_mkCodeForTags = const "",
    lc_rewriteLine = id
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
      in takeDirectory fp </> "__Class_" ++ map replace (takeFileName fp)
  , lc_extraArgs = ["method", "body", "static", "test"]
  , lc_tagForCall = \call -> do
      let loc = pc_location call
          m = pc_args call
      method <- fromMaybe False <$> getOptionalBoolValue loc "method" m
      body <- tagForArg "body" m call
      test <- tagForArg "test" m call
      let onlyOne = Left (unLocation loc <> ": at most one of method, body, and test allowed")
      case (method, body, test) of
        (False, Just body, Nothing) -> Right (Just body)
        (False, Nothing, Just test) -> Right (Just test)
        (True, Nothing, Nothing) -> Right (Just (methodTag (hash call)))
        (False, Nothing, Nothing) -> Right Nothing
        (True, _, Just _) -> onlyOne
        (True, Just _, _) -> onlyOne
        (_, Just _, Just _) -> onlyOne
  , lc_mkCodeForTags = \m' ->
      let m = groupByTag m'
          methods = map snd $ filter (\(t, _) -> t_kind t == "method") m
          bodiesOrTests k = map (Data.Bifunctor.first t_id) $ filter (\(t, _) -> t_kind t == k) m
          bodies = bodiesOrTests "body"
          tests = bodiesOrTests "test"
          methodsForBodies = flip map bodies $ \(id, code) ->
            "public static void __body_" <> id <> "() throws Exception {\n" <> code <> "\n}"
          methodsForTests = flip map tests $ \(id, code) ->
            "@Test public void __body_" <> id <> "() throws Exception {\n" <> code <> "\n}"
          allMethods = methods ++ methodsForBodies ++ methodsForTests
      in if null allMethods
            then ""
            else let code = T.concat (L.intersperse "\n\n" allMethods)
                 in "class __CodeContainer {\n" <> code <> "\n}"
  , lc_rewriteLine = rewrite
  }
  where
    methodTag = Tag "method"
    hash c = unHash (md5OfText (unLocation (pc_location c)))
    tagForArg arg m call = do
      case M.lookup arg m of
        Nothing -> pure Nothing
        Just (ArgString t) -> pure $ Just (Tag arg t)
        Just (ArgInt i) -> pure $ Just (Tag arg (showText i))
        Just (ArgBool True) -> pure $ Just (Tag arg (hash call))
        Just (ArgBool False) -> pure Nothing
    methodReturnType line =
      let ws = T.words (T.strip line)
          isKw x = x `elem` ["public", "private", "protected", "final", "abstract", "static"]
      in case filter (not . isKw) ws of
           [] -> Nothing
           l ->
             case T.words (T.pack (dropTyArgs 0 (T.unpack (T.unwords l)))) of
               [] -> Nothing
               (x:_) -> Just x
    dropTyArgs i ('<':rest) = dropTyArgs (i+1) rest
    dropTyArgs i ('>':rest) = dropTyArgs (i-1) rest
    dropTyArgs 0 s = s
    dropTyArgs i (_:rest) = dropTyArgs i rest
    dropTyArgs _ [] = []
    rewrite line =
      case methodReturnType line of
        Nothing -> line
        Just ty ->
          let repl =
                case ty of
                  "void" -> "{ }"
                  "boolean" -> "{ return false; }"
                  "int" -> "{ return 0; }"
                  "short" -> "{ return 0; }"
                  "byte" -> "{ return 0; }"
                  "long" -> "{ return 0L; }"
                  "double" -> "{ return 0.0; }"
                  "float" -> "{ return 0.0f; }"
                  "char" -> "{ return '\\0'; }"
                  _ -> "{ return null; }"
          in T.replace "{ ... }" repl (T.replace "{...}" repl line)

lineComment :: LangConfig -> T.Text -> T.Text
lineComment cfg t = lc_commentStart cfg <> t <> fromMaybe "" (lc_commentEnd cfg)

codePlugins :: [ExternalLangConfig] -> [AnyPluginConfig Action]
codePlugins externalLangs =
  let languages =
        map languageFromExternal onlyExternalLangs ++
        map combineLanguage langPairs ++
        onlyInternalLangs
  in flip map languages $ \(name, langCfg) ->
      AnyPluginConfig $ mkCodePlugin (PluginName name) langCfg
  where
    langPairs =
      flip mapMaybe externalLangs $ \ext -> do
        int <- L.lookup (elc_name ext) languages
        pure (ext, int)
    onlyExternalLangs =
      flip filter externalLangs $ \ext -> isNothing  (L.lookup (elc_name ext) languages)
    onlyInternalLangs =
      flip filter languages $ \(name, _) ->
        not (any (\ext -> elc_name ext == name) externalLangs)
    combineLanguage (ext, cfg) =
      ( elc_name ext,
        cfg
        { lc_fileExt = elc_fileExt ext
        , lc_commentStart = elc_commentStart ext
        , lc_commentEnd = elc_commentEnd ext
        }
      )
    languageFromExternal cfg =
      ( elc_name cfg,
        mkLangConfig (elc_fileExt cfg) (elc_commentStart cfg) (elc_commentEnd cfg)
      )

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
