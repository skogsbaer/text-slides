module BuildConfig
  ( getBuildConfig,
  )
where

import qualified Data.Map.Strict as M
import Development.Shake
import Plugins.Code
import Plugins.Keynote
import Plugins.Mermaid
import Types

allPlugins :: [PluginConfig Action]
allPlugins = [keynotePlugin, mermaidPlugin] ++ codePlugins

defaultBuildConfig :: BuildConfig
defaultBuildConfig =
  BuildConfig
    { bc_buildDir = "build",
      bc_pandoc = "pandoc",
      bc_python = "python3",
      bc_convert = "convert",
      bc_mermaid = "mmdc",
      bc_plugins = M.fromList $ map (\p -> (p_name p, p)) allPlugins
    }

getBuildConfig :: IO BuildConfig
getBuildConfig =
  -- simple for now, will extend with values read from config file later
  return defaultBuildConfig
