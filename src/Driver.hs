{-# LANGUAGE MultiWayIf #-}

module Driver (main) where

import BuildConfig
import Cmdline
import Control.Monad
import CoreRules
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Development.Shake hiding (doesDirectoryExist, doesFileExist)
import Logging
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf
import Types
import Utils

shakeProfileBaseFile :: String
shakeProfileBaseFile = "shake-profile."

shakeProfileFiles :: [FilePath]
shakeProfileFiles =
  map (\x -> shakeProfileBaseFile ++ x) ["html", "json", "trace"]

-- We use a very simple approach to compute the shake version number: we hash the build config
-- and the executable. Thus, changing either the code or the build config forces a full
-- rebuild.
getShakeVersion :: StaticBuildConfig -> IO String
getShakeVersion cfg = do
  exe <- getExecutablePath
  h1 <- md5OfFile exe
  let hash = md5OfText (showText h1 <> showText cfg)
  return $ T.unpack (unHash hash)

cleanupShakeFiles :: FilePath -> String -> IO ()
cleanupShakeFiles shakeDir curVersion = do
  b <- doesDirectoryExist shakeDir
  when b $ do
    files <- myListDirectory shakeDir (const True)
    forM_ files $ \file -> do
      b <- doesDirectoryExist file
      when (b && takeFileName file /= curVersion) $ do
        noteIO $ "Deleting old shake directory " ++ file
        removeDirectoryRecursive file

main :: IO ()
main = do
  opts <- parseCmdlineArgs
  args <- computeBuildArgs opts
  let logLevel =
        if
            | co_debug opts -> DEBUG
            | co_verbose opts -> INFO
            | co_quiet opts -> WARN
            | otherwise -> NOTE
  setLogLevel logLevel
  let targets =
        flip map (S.toList (co_outputs opts)) $ \mode ->
          mainOutputFile args (T.unpack $ outputModeToExtension mode)
  noteIO $ "Welcome to text-slides. Bringing " ++ (L.intercalate ", " targets) ++ " up-to-date"
  shakeVersion <- liftIO $ getShakeVersion staticBuildConfig
  -- If the shake version changes, I get the following problem when running text-slides:
  -- text-slides: build/.shake/.shake.database: hSetFileSize: illegal operation (handle is closed)
  -- Maybe this is a bug in shake. As a workaround, I include the shake version in the path
  -- to the shake database and cleanup old version here.
  cleanupShakeFiles (shakeDir staticBuildConfig) shakeVersion
  (secs, _) <- withTiming $
    shake (mkShakeOptions staticBuildConfig opts shakeVersion) $ do
      _ <- addOracle $ \(BuildConfigKey _) -> computeBuildConfig opts args
      want targets
      coreRules args
  noteIO $ printf "Everything is up-to-date now, total duration: %.3fs" secs
  where
    shakeDir cfg = sbc_buildDir cfg </> ".shake"
    mkShakeOptions cfg opts v =
      shakeOptions
        { shakeVerbosity = co_shakeVerbosity opts,
          shakeVersion = v,
          shakeReport =
            if co_shakeProfile opts
              then shakeProfileFiles
              else [],
          shakeThreads = co_jobs opts,
          shakeLint = Nothing, -- Just LintBasic
          shakeFiles = shakeDir cfg </> v,
          shakeFlush = Just 1.0,
          shakeChange = ChangeModtimeAndDigest,
          shakeStorageLog = True
        }
