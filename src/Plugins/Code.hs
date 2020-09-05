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

data CodeArgs = CodeArgs
  {ca_file :: Maybe FilePath}

parseArgs :: PluginName -> ArgMap -> Fail CodeArgs
parseArgs plugin m = do
  file <- case M.lookup "file" m of
    Just (ArgString t) -> Right (Just (T.unpack t))
    Just _ -> Left ("Argument file of plugin " <> unPluginName plugin <> " must be a string")
    Nothing -> Right Nothing
  return $ CodeArgs {ca_file = file}

mkCodePlugin :: PluginName -> LangConfig -> PluginConfig Action
mkCodePlugin name cfg =
  PluginConfig
    { p_name = name,
      p_kind = PluginWithBody,
      p_rules = pluginRules,
      p_expand = runPlugin,
      p_forAllCalls = processAllCalls cfg
    }

pluginRules :: Rules ()
pluginRules = return ()

runPlugin :: PluginCall -> ExceptT T.Text Action T.Text
runPlugin call = return (pc_body call)

processAllCalls ::
  LangConfig -> BuildConfig -> BuildArgs -> [PluginCall] -> ExceptT T.Text Action ()
processAllCalls langCfg cfg buildArgs calls = do
  codeMap <- exceptInM $ foldM collectCode M.empty calls
  forM_ (M.toList codeMap) $ \(file, code) -> do
    lift $ note ("Generating " ++ file)
    lift $ myWriteFile file code
  where
    collectCode m call = do
      (file, code) <- codeFromCall call
      return $ M.insertWith (\new old -> old <> "\n\n" <> new) file code m
    codeFromCall :: PluginCall -> Fail (FilePath, T.Text)
    codeFromCall call = do
      args <- parseArgs (pc_pluginName call) (pc_args call)
      let baseFile =
            case ca_file args of
              Nothing -> replaceExtension (ba_inputFile buildArgs) (lc_fileExt langCfg)
              Just f -> f
          file = pluginDir cfg (pc_pluginName call) </> baseFile
          body = T.stripEnd (pc_body call)
          code = lineComment langCfg (pc_location call) <> body
      return (file, code)

data LangConfig = LangConfig
  { lc_fileExt :: String,
    lc_commentStart :: T.Text,
    lc_commentEnd :: Maybe T.Text
  }

lineComment :: LangConfig -> T.Text -> T.Text
lineComment cfg t = lc_commentStart cfg <> t <> fromMaybe "" (lc_commentEnd cfg) <> "\n"

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
