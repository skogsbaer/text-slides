module BuildConfig
  ( getBuildConfig,
  )
where

import Cmdline
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import Development.Shake hiding (doesFileExist)
import Logging
import Plugins.Code
import Plugins.Keynote
import Plugins.Mermaid
import System.Directory
import System.FilePath
import Types

allPlugins :: [AnyPluginConfig Action]
allPlugins = [keynotePlugin, mermaidPlugin] ++ codePlugins

defaultBuildConfig :: BuildConfig
defaultBuildConfig =
  BuildConfig
    { bc_buildDir = "build",
      bc_pandoc = "pandoc",
      bc_python = "python3",
      bc_convert = "convert",
      bc_mermaid = "mmdc",
      bc_beamerHeader = Nothing,
      bc_plugins =
        M.fromList $ map (\(AnyPluginConfig p) -> (p_name p, AnyPluginConfig p)) allPlugins
    }

getBuildConfig :: CmdlineOpts -> IO BuildConfig
getBuildConfig opts = do
  beamerHeader <- searchFile "beamer-header.tex" (co_beamerHeader opts) >>= canonicalize
  infoIO ("beamerHeader: " ++ show beamerHeader)
  return $ defaultBuildConfig {bc_beamerHeader = beamerHeader}
  where
    canonicalize Nothing = return Nothing
    canonicalize (Just p) = do
      cp <- canonicalizePath p
      return (Just cp)
    searchFile :: FilePath -> Maybe FilePath -> IO (Maybe FilePath)
    searchFile _ (Just fromCmdLine) = return $ Just fromCmdLine
    searchFile path Nothing = do
      home <- getHomeDirectory
      current <- getCurrentDirectory
      let homeCfgDir = home </> ".text-slides"
          candidates =
            ( case co_inputFile opts of
                Just f -> [takeDirectory f </> path]
                Nothing -> [current </> path]
            )
              ++ [homeCfgDir </> path]
      results <- forM candidates $ \cand -> do
        b <- doesFileExist cand
        return $ if b then Just cand else Nothing
      case catMaybes results of
        [] -> return Nothing
        (x : _) -> return $ Just x
