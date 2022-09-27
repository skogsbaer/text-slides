{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Plugins
  ( htf_thisModulesTests,
  )
where

import Cmdline
import qualified Data.List as L
import qualified Data.Set as S
import Driver
import System.Directory
import System.FilePath
import Test.Framework
import Types
import Utils
import Temp

runTextSlides :: FilePath -> (FilePath -> IO ()) -> IO ()
runTextSlides mdFile checkFun = withSysTempDir deleteIfNoException "text-slides-test" $ \dir -> do
  copyFile mdFile (dir </> takeBaseName mdFile)
  let opts =
        emptyCmdlineOpts
          { co_inputFile = Just (takeBaseName mdFile),
            co_outputs = S.fromList [OutputHtml]
          }
  withCurrentDirectory dir (mainWithOpts opts)
  checkFun dir

test_java :: IO ()
test_java = runTextSlides "test/data/test_java.md" $ \outDir -> do
  let pluginDir = outDir </> "build/plugins/java/"
      referenceDir = "test/data/test_java"
  genFiles <- myListDirectoryRecursive outDir (\p -> ".java" `L.isSuffixOf` p)
  let expectedFiles = javaFiles pluginDir
  assertListsEqualAsSets expectedFiles genFiles
  let _referenceFiles = javaFiles referenceDir
  _ <- fail "implement test_java"
  return ()
  where
    javaFiles dir =
      let outAll = dir </> "test_java.java"
          outDef1 = dir </> "default_pkg/01/C1.java"
          outDef2 = dir </> "default_pkg/02/C1.java"
          outFoo1 = dir </> "foo/01/foo/Main.java"
          outFoo2 = dir </> "foo/02/foo/C1.java"
          outFoo3 = dir </> "foo/03/foo/C2.java"
          outFoo4 = dir </> "foo/04/foo/C2.java"
          outFoo5 = dir </> "foo/05/foo/C2.java"
          outFoo6 = dir </> "foo/06/foo/C2.java"
          outFoo7 = dir </> "foo/07/foo/C2.java"
          outFoo8 = dir </> "foo/08/foo/C2.java"
          outAlt1 = dir </> "alternative/default_pkg_01/01/C1.java"
          outAlt2 = dir </> "alternative/default_pkg_01/02/C1.java"
          outAlt3 = dir </> "alternative/default_pkg_02/01/C1.java"
       in [ outAll,
            outDef1,
            outDef2,
            outFoo1,
            outFoo2,
            outFoo3,
            outFoo4,
            outFoo5,
            outFoo6,
            outFoo7,
            outFoo8,
            outAlt1,
            outAlt2,
            outAlt3
          ]
