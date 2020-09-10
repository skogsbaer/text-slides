{-

A plugin call such as

~~~python
BODY
~~~

simply places to same content in the markdown file. The p_forAllCalls action (invoked when
generating the main output document) then writes to code snippets into the appropriate
files.
-}
module Plugins.Code (codePlugins) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Development.Shake
import Logging
import System.FilePath
import Types
import Utils

data CodeMode = CodeModeShow | CodeModeHide | CodeModeShowOnly
  deriving (Eq, Show)

data LineNumberMode = LineNumbersOff | LineNumbersOn | LineNumbersAuto
  deriving (Eq, Show)

data FirstLine = FirstLineImplicit | FirstLineExplicit Int | FirstLineContinue
  deriving (Eq, Show)

data CodeArgs = CodeArgs
  { ca_file :: Maybe FilePath,
    ca_mode :: CodeMode,
    ca_lineNumberMode :: LineNumberMode,
    ca_firstLine :: FirstLine
  }

parseArgs :: PluginCall -> Fail CodeArgs
parseArgs call = do
  file <- getOptionalStringValue loc "file" m
  modeStr <- getOptionalEnumValue loc "mode" ["show", "hide", "showOnly"] m
  lineNumStr <- getOptionalEnumValue loc "lineNumbers" ["on", "off", "auto"] m
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
  firstLineM <- getOptionalValue loc "firstLine" m "Int or \"continue\"" $ \v ->
    case v of
      ArgInt i -> Just $ FirstLineExplicit i
      ArgString "continue" -> Just $ FirstLineContinue
      _ -> Nothing
  let firstLine = fromMaybe FirstLineImplicit firstLineM
  checkForSpuriousArgs loc m ["file", "mode", "lineNumbers", "firstLine"]
  return $
    CodeArgs
      { ca_file = fmap T.unpack file,
        ca_mode = mode,
        ca_lineNumberMode = lineNum,
        ca_firstLine = firstLine
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
      p_expand = runPlugin,
      p_forAllCalls = processAllCalls cfg
    }

pluginRules :: BuildConfig -> BuildArgs -> Rules ()
pluginRules _cfg _args = return ()

runPlugin ::
  BuildConfig -> BuildArgs -> CodeState -> PluginCall -> ExceptT T.Text Action (T.Text, CodeState)
runPlugin _cfg _buildArgs state call = do
  args <- exceptInM $ parseArgs call
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

processAllCalls ::
  LangConfig -> BuildConfig -> BuildArgs -> [PluginCall] -> ExceptT T.Text Action ()
processAllCalls langCfg cfg buildArgs calls = do
  codeMap <- exceptInM $ foldM collectCode M.empty calls
  forM_ (M.toList codeMap) $ \(file, code) -> do
    lift $ note ("Generating " ++ file)
    lift $ myWriteFile file code
  where
    collectCode m call = do
      (file, mCode) <- codeFromCall call
      case mCode of
        Nothing -> return m
        Just code -> return $ M.insertWith (\new old -> old <> "\n\n" <> new) file code m
    codeFromCall :: PluginCall -> Fail (FilePath, Maybe T.Text)
    codeFromCall call = do
      args <- parseArgs call
      let baseFile =
            case ca_file args of
              Nothing -> replaceExtension (ba_inputFile buildArgs) (lc_fileExt langCfg)
              Just f -> f
          file = pluginDir cfg (pc_pluginName call) </> baseFile
          body = T.stripEnd (pc_body call)
          code = lineComment langCfg (pc_location call) <> body
      let mCode =
            case ca_mode args of
              CodeModeShowOnly -> Nothing
              _ -> Just code
      return (file, mCode)

data LangConfig = LangConfig
  { lc_fileExt :: String,
    lc_commentStart :: T.Text,
    lc_commentEnd :: Maybe T.Text
  }

lineComment :: LangConfig -> Location -> T.Text
lineComment cfg l = lc_commentStart cfg <> unLocation l <> fromMaybe "" (lc_commentEnd cfg) <> "\n"

codePlugins :: [AnyPluginConfig Action]
codePlugins = flip map languages $ \(name, langCfg) ->
  AnyPluginConfig $ mkCodePlugin (PluginName name) langCfg

languages :: [(T.Text, LangConfig)]
languages =
  [ ("bash", LangConfig ".sh" "# " Nothing),
    ("c", LangConfig ".c" "// " Nothing),
    ("cs", LangConfig ".cs" "// " Nothing),
    ("css", LangConfig ".css" "// " Nothing),
    ("clojure", LangConfig ".clj" ";; " Nothing),
    ("erlang", LangConfig ".erl" "% " Nothing),
    ("fsharp", LangConfig ".fs" "// " Nothing),
    ("html", LangConfig ".html" "<!-- " (Just " -->")),
    ("haskell", LangConfig ".hs" "-- " Nothing),
    ("json", LangConfig ".json" "// " Nothing),
    ("java", LangConfig ".java" "// " Nothing),
    ("javascript", LangConfig ".js" "// " Nothing),
    ("typescript", LangConfig ".ts" "// " Nothing),
    ("ocaml", LangConfig ".ml" "(* " (Just " *)")),
    ("objectivec", LangConfig ".m" "// " Nothing),
    ("python", LangConfig ".py" "# " Nothing),
    ("rust", LangConfig ".rs" "// " Nothing),
    ("scheme", LangConfig ".sc" ";; " Nothing),
    ("xml", LangConfig ".xml" "<!-- " (Just " -->"))
  ]
