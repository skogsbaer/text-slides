module Test.TestBuildConfig where

import qualified Data.Map as M
import Types

testBuildConfig :: [PluginConfig m] -> GenericBuildConfig m
testBuildConfig plugins =
  BuildConfig
    { bc_buildDir = "build",
      bc_pandoc = "pandoc",
      bc_python = "python3",
      bc_plugins = M.fromList $ map (\p -> (p_name p, p)) plugins
    }

testBuildArgs :: BuildArgs
testBuildArgs = BuildArgs {ba_inputFile = "sample.md"}
