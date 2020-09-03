module BuildConfig (
    getBuildConfig
) where

import Types

defaultBuildConfig :: BuildConfig
defaultBuildConfig =
    BuildConfig
    { bc_buildDir = "./build"
    , bc_pandoc = "pandoc"
    }

getBuildConfig :: IO BuildConfig
getBuildConfig =
    -- simple for now, will extend with values read from config file later
    return defaultBuildConfig
