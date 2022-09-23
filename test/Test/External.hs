{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.External
  ( htf_thisModulesTests,
  )
where

import Cmdline
import qualified Data.Set as S
import Driver
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Framework
import Types

runExternal :: FilePath -> (FilePath -> IO ()) -> IO ()
runExternal mdFile checkFun = withTempDirectory "/tmp" "text-slides-test" $ \dir -> do
  -- FIXME: do not delete temp file in case of errors
  copyFile mdFile (dir </> takeBaseName mdFile)
  let opts =
        emptyCmdlineOpts
          { co_inputFile = Just (takeBaseName mdFile),
            co_outputs = S.fromList [OutputHtml]
          }
  withCurrentDirectory dir (mainWithOpts opts)
  checkFun dir

test_java :: IO ()
test_java = runExternal "test/data/test_java.md" $ \_dir -> do
  let pluginDir = "build/plugins/java/"
      outAll = pluginDir </> "test_java.java"
      outDef1 = pluginDir </> "default_pkg/01/C1.java"
      outDef2 = pluginDir </> "default_pkg/02/C1.java"
      outFoo1 = pluginDir </> "foo/01/foo/Main.java"
      outFoo2 = pluginDir </> "foo/02/foo/C1.java"
      outFoo3 = pluginDir </> "foo/03/foo/C2.java"
      outFoo4 = pluginDir </> "foo/04/foo/C2.java"
      outFoo5 = pluginDir </> "foo/05/foo/C2.java"
      outFoo6 = pluginDir </> "foo/06/foo/C2.java"
      outFoo7 = pluginDir </> "foo/07/foo/C2.java"
      outFoo8 = pluginDir </> "foo/08/foo/C2.java"
      outAlt1 = pluginDir </> "alternative/default_pkg_01/01/C1.java"
      outAlt2 = pluginDir </> "alternative/default_pkg_01/02/C1.java"
      outAlt3 = pluginDir </> "alternative/default_pkg_02/01/C1.java"
  fail "implement test_java"
  return ()
