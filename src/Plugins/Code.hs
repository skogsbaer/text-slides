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

data CodeArgs = CodeArgs
  { ca_file :: Maybe FilePath,
    ca_mode :: CodeMode
  }

parseArgs :: PluginCall -> Fail CodeArgs
parseArgs call = do
  file <- getOptionalStringValue loc "file" m
  modeStr <- getOptionalEnumValue loc "mode" ["show", "hide", "showOnly"] m
  mode <-
    case modeStr of
      Just "show" -> return CodeModeShow
      Just "hide" -> return CodeModeHide
      Just "showOnly" -> return CodeModeShowOnly
      Nothing -> return CodeModeShow
      Just _ -> error $ "uncovered case: " ++ show modeStr
  checkForSpuriousArgs loc m ["file", "mode"]
  return $ CodeArgs {ca_file = fmap T.unpack file, ca_mode = mode}
  where
    loc = pc_location call
    m = pc_args call

mkCodePlugin :: PluginName -> LangConfig -> PluginConfig Action
mkCodePlugin name cfg =
  PluginConfig
    { p_name = name,
      p_kind = PluginWithBody,
      p_rules = pluginRules,
      p_expand = runPlugin,
      p_forAllCalls = processAllCalls cfg
    }

pluginRules :: BuildConfig -> BuildArgs -> Rules ()
pluginRules _cfg _args = return ()

runPlugin :: BuildConfig -> BuildArgs -> PluginCall -> ExceptT T.Text Action T.Text
runPlugin _cfg _buildArgs call = do
  args <- exceptInM $ parseArgs call
  let code = "~~~" <> unPluginName (pc_pluginName call) <> "\n" <> pc_body call <> "\n~~~"
  case ca_mode args of
    CodeModeShow -> return code
    CodeModeShowOnly -> return code
    CodeModeHide -> return ""

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

codePlugins :: [PluginConfig Action]
codePlugins = flip map languages $ \(name, langCfg) ->
  mkCodePlugin (PluginName name) langCfg

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
