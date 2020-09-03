{-# LANGUAGE MultiWayIf #-}
module Driver (main) where

import Types
import BuildConfig
import CoreRules
import Cmdline
import Logging
import Development.Shake
import System.FilePath
import qualified Data.Set as Set
import qualified Data.Text as T

-- | The version number of your build rules. Change the version number to force a complete
-- rebuild, such as when making significant changes to the rules that require a wipe.
myShakeVersion :: String
myShakeVersion = "2020-09-03"

shakeProfileBaseFile :: String
shakeProfileBaseFile = "shake-profile."

shakeProfileFiles :: [FilePath]
shakeProfileFiles =
    map (\x -> shakeProfileBaseFile ++ x) ["html", "json", "trace"]

main :: IO ()
main = do
  opts <- parseCmdlineArgs
  let logLevel =
        if
            | co_debug opts -> DEBUG
            | co_verbose opts -> INFO
            | co_quiet opts -> WARN
            | otherwise -> NOTE
  setLogLevel logLevel
  cfg <- getBuildConfig
  let args =
        BuildArgs
        { ba_inputFile = co_inputFile opts }
      resolvedTargets =
        flip map (Set.toList (co_outputs opts)) $ \mode ->
            bc_buildDir cfg </>
            replaceExtension (co_inputFile opts) (T.unpack $ outputModeToExtension mode)
  shake (mkShakeOptions cfg opts) $ do
    want resolvedTargets
    coreRules cfg args
  where
    mkShakeOptions cfg opts =
      shakeOptions
        { shakeVerbosity = co_shakeVerbosity opts,
          shakeVersion = myShakeVersion,
          shakeReport =
            if co_shakeProfile opts
              then shakeProfileFiles
              else [],
          shakeThreads = co_jobs opts,
          shakeLint = Nothing, -- Just LintBasic
          shakeFiles = bc_buildDir cfg </> ".shake",
          shakeFlush = Just 1.0,
          shakeChange = ChangeModtimeAndDigest,
          shakeStorageLog = True
        }
