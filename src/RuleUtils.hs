module RuleUtils where

import qualified Data.List as L
import qualified Data.Text as T
import Development.Shake
import System.FilePath
import Types
import Utils

depsFile :: FilePath -> FilePath
depsFile fp = fp -<.> "deps"

writeDeps :: FilePath -> [FilePath] -> Action ()
writeDeps fp deps = do
  myWriteFile (depsFile fp) $ T.unlines $ map T.pack deps

readDeps :: FilePath -> Action [FilePath]
readDeps fp = do
  deps <- myReadFile (depsFile fp)
  return $ map T.unpack $ filter (not . T.null) $ map T.strip $ T.lines deps

outputFileToInputFile :: BuildConfig -> BuildArgs -> FilePath -> FilePath
outputFileToInputFile cfg args out =
  let prefix = bc_buildDir cfg ++ "/"
   in if not (prefix `L.isPrefixOf` out)
        then error ("outputFileToInputFile: not an output file: " ++ out)
        else ba_searchDir args </> drop (length prefix) out

-- To keep things simple, the output file for the presentation is always placed at the
-- toplevel of the build dir. This means, that two input files must not have the same
-- basename.
mainOutputFile :: BuildConfig -> BuildArgs -> String -> FilePath
mainOutputFile cfg args ext =
  bc_buildDir cfg </> takeBaseName (ba_inputFile args) <.> ext

