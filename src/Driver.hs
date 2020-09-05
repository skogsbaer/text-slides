{-# LANGUAGE MultiWayIf #-}

module Driver (main) where

import BuildConfig
import Cmdline
import CoreRules
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Development.Shake
import Logging
import System.FilePath
import Types

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
          { ba_inputFile = co_inputFile opts
          }
      targets =
        flip map (S.toList (co_outputs opts)) $ \mode ->
          bc_buildDir cfg
            </> replaceExtension (co_inputFile opts) (T.unpack $ outputModeToExtension mode)
  noteIO $ "Welcome to text-slides. Bringing " ++ (L.intercalate ", " targets) ++ " up-to-date"
  shake (mkShakeOptions cfg opts) $ do
    want targets
    coreRules cfg args
  noteIO $ "Everything is up-to-date now."
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
