module Test.TestBuildConfig where

import qualified Data.Map as M
import Types

testBuildConfig :: [AnyPluginConfig m] -> GenericBuildConfig m
testBuildConfig plugins =
  BuildConfig
    { bc_buildDir = "build",
      bc_pandoc = "pandoc",
      bc_python = "python3",
      bc_convert = "convert",
      bc_mermaid = "mermaid",
      bc_beamerHeader = Nothing,
      bc_plugins = M.fromList $ map (\(AnyPluginConfig p) -> (p_name p, AnyPluginConfig p)) plugins
    }

testBuildArgs :: BuildArgs
testBuildArgs = BuildArgs {ba_inputFile = "sample.md"}
