{-# LANGUAGE LambdaCase #-}

{-

A plugin call such as

~~~python
BODY
~~~

simply places the same content in the markdown file. The p_forAllCalls action (invoked when
generating the main output document) then writes to code snippets into the appropriate
files.
-}

module Plugins.Code (codePlugins, mkLangConfig, ExternalLangConfig (..)) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock
import Development.Shake
import Logging
import Plugins.CodeCommon
import Plugins.JavaCode
import System.FilePath
import Types
import Utils

data CodeMode = CodeModeShow | CodeModeHide | CodeModeShowOnly
  deriving (Eq, Show)

data LineNumberMode = LineNumbersOff | LineNumbersOn | LineNumbersAuto
  deriving (Eq, Show)

data FirstLine = FirstLineImplicit | FirstLineExplicit Int | FirstLineContinue
  deriving (Eq, Show)

data CodePlace = AtStart | Here | AtEnd
  deriving (Eq, Show)

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
  checkForSpuriousArgs
    loc
    m
    ( ["file", "mode", "lineNumbers", "firstLine", "place", "comment", "prepend"]
        ++ extraArgs
    )
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
      p_init = return initialCodeState,
      p_expand = runPlugin cfg,
      p_forAllCalls = processAllCalls cfg
    }

-- Execute a plugin call. Extracts the code for presentation.
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
      body = extractCode langCfg CodeExctractPresentation (Code (pc_body call))
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

resolveCodeFilePath :: BuildArgs -> LangConfig -> CodeFilePath -> FilePath
resolveCodeFilePath buildArgs langCfg cfp =
  let base =
        case cfp of
          CodeFilePathCustom fp -> fp
          CodeFilePathDefault ->
            replaceExtension (ba_inputFile buildArgs) (lc_fileExt langCfg)
   in pluginDir (PluginName (lc_name langCfg)) </> base

emptyCodeSnippetFile :: CodeSnippetFile
emptyCodeSnippetFile = CodeSnippetFile [] [] []

mkCodeSnippetFile :: CodeSnippet -> CodePlace -> CodeSnippetFile
mkCodeSnippetFile cc place =
  case place of
    AtStart -> emptyCodeSnippetFile {ccf_atStart = [cc]}
    Here -> emptyCodeSnippetFile {ccf_here = [cc]}
    AtEnd -> emptyCodeSnippetFile {ccf_atEnd = [cc]}

appendCodeSnippet :: CodeSnippet -> CodePlace -> CodeSnippetFile -> CodeSnippetFile
appendCodeSnippet cc place file =
  case place of
    AtStart -> file {ccf_atStart = cc : ccf_atStart file}
    Here -> file {ccf_here = cc : ccf_here file}
    AtEnd -> file {ccf_atEnd = cc : ccf_atEnd file}

defaultProcessCodeMap ::
  BuildConfig ->
  BuildArgs ->
  LangConfig ->
  T.Text ->
  M.Map CodeFilePath CodeSnippetFile ->
  Action ()
defaultProcessCodeMap _cfg buildArgs langCfg header codeMap =
  forM_ (M.toList codeMap) $ \(codeFile, ccf) -> do
    let file = resolveCodeFilePath buildArgs langCfg codeFile
        code =
          mkCode langCfg (ccf_atStart ccf)
            <> mkCode langCfg (ccf_here ccf)
            <> mkCode langCfg (ccf_atEnd ccf)
    note ("Generating " ++ file)
    myWriteFile file (header <> code)

processAllCalls ::
  LangConfig -> BuildConfig -> BuildArgs -> [PluginCall] -> ExceptT T.Text Action ()
processAllCalls langCfg cfg buildArgs calls = do
  let allPlugins = Set.fromList (map pc_pluginName calls)
  forM_ allPlugins $ \pluginName -> do
    let dir = pluginDir pluginName
    liftIO $ removeAllFilesInDirectory dir `catch` (\(_ :: IOError) -> pure ())
  codeMap <- exceptInM $ collectCode calls
  time <- liftIO getCurrentTime
  let header =
        cmt
          ( "Automatically extracted from " <> T.pack (ba_inputFile buildArgs)
              <> " on "
              <> showText time
              <> "\n\n"
          )
  lift $ lc_processCodeMap langCfg cfg buildArgs langCfg header codeMap
  where
    cmt = lineComment langCfg
    collectCode :: [PluginCall] -> Fail CodeMap
    collectCode calls = do
      revMap <- foldM collectCode' M.empty calls
      return $ M.map (applyToCodeSnippets reverse) revMap
    collectCode' :: CodeMap -> PluginCall -> Fail CodeMap
    collectCode' m call = do
      (file, mCode) <- codeFromCall call
      case mCode of
        Nothing -> return m
        Just (place, code) ->
          let cc =
                CodeSnippet
                  { cc_code = code,
                    cc_sectionName = pc_sectionName call,
                    cc_location = pc_location call,
                    cc_args = pc_args call
                  }
           in case M.lookup file m of
                Just ccf -> return $ M.insert file (appendCodeSnippet cc place ccf) m
                Nothing -> return $ M.insert file (mkCodeSnippetFile cc place) m
    codeFromCall :: PluginCall -> Fail (CodeFilePath, Maybe (CodePlace, Code))
    codeFromCall call = do
      args <- parseArgs call (lc_extraArgs langCfg)
      let file =
            case ca_file args of
              Nothing -> CodeFilePathDefault
              Just f -> CodeFilePathCustom f
          body =
            maybe "" (\t -> t <> "\n") (ca_prepend args)
              <> (comment (ca_comment args) $ T.stripEnd (pc_body call))
          code =
            Code $
              "\n" <> cmt ("[" <> unLocation (pc_location call) <> "]") <> "\n" <> body
      mCode <-
        case ca_mode args of
          CodeModeShowOnly -> return Nothing
          _ -> return $ Just (ca_place args, code)
      return (file, mCode)
    comment False t = t
    comment True t =
      T.unlines $
        map (lineComment langCfg) $
          T.lines t

codePlugins :: [ExternalLangConfig] -> [AnyPluginConfig Action]
codePlugins externalLangs =
  let languages =
        map languageFromExternal onlyExternalLangs
          ++ map combineLanguage internalAndExternalLangs
          ++ onlyInternalLangs
   in flip map languages $ \langCfg ->
        AnyPluginConfig $ mkCodePlugin (PluginName (lc_name langCfg)) langCfg
  where
    internalAndExternalLangs =
      flip mapMaybe externalLangs $ \ext -> do
        int <- L.find (\l -> elc_name ext == lc_name l) languages
        pure (ext, int)
    onlyExternalLangs =
      flip filter externalLangs $
        \ext -> isNothing (L.find (\l -> elc_name ext == lc_name l) languages)
    onlyInternalLangs =
      flip filter languages $ \l ->
        not (any (\ext -> elc_name ext == lc_name l) externalLangs)
    combineLanguage (ext, cfg) =
      cfg
        { lc_fileExt = elc_fileExt ext,
          lc_commentStart = elc_commentStart ext,
          lc_commentEnd = elc_commentEnd ext
        }
    languageFromExternal cfg =
      mkLangConfig
        (elc_name cfg)
        (elc_fileExt cfg)
        (elc_commentStart cfg)
        (elc_commentEnd cfg)
        defaultProcessCodeMap

languages :: [LangConfig]
languages =
  [ mkLangConfig "bash" ".sh" "# " Nothing defaultProcessCodeMap,
    mkLangConfig "c" ".c" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "cs" ".cs" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "css" ".css" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "clojure" ".clj" ";; " Nothing defaultProcessCodeMap,
    mkLangConfig "erlang" ".erl" "% " Nothing defaultProcessCodeMap,
    mkLangConfig "fsharp" ".fs" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "html" ".html" "<!-- " (Just " -->") defaultProcessCodeMap,
    mkLangConfig "haskell" ".hs" "-- " Nothing defaultProcessCodeMap,
    mkLangConfig "json" ".json" "// " Nothing defaultProcessCodeMap,
    javaLangConfig,
    mkLangConfig "javascript" ".js" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "typescript" ".ts" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "ocaml" ".ml" "(* " (Just " *") defaultProcessCodeMap,
    mkLangConfig "objectivec" ".m" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "python" ".py" "# " Nothing defaultProcessCodeMap,
    mkLangConfig "rust" ".rs" "// " Nothing defaultProcessCodeMap,
    mkLangConfig "scheme" ".sc" ";; " Nothing defaultProcessCodeMap,
    mkLangConfig "xml" ".xml" "<!-- " (Just " -->") defaultProcessCodeMap
  ]
