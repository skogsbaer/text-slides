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
import Data.Maybe
import System.Process
import System.Exit
import Control.Monad

runTextSlides :: FilePath -> (FilePath -> IO ()) -> IO ()
runTextSlides mdFile checkFun = withSysTempDir deleteIfNoException "text-slides-test" $ \dir -> do
  copyFile mdFile (dir </> takeFileName mdFile)
  let opts =
        emptyCmdlineOpts
          { co_inputFile = Just (takeFileName mdFile),
            co_outputs = S.fromList [OutputHtml]
          }
  withCurrentDirectory dir (mainWithOpts opts)
  checkFun dir

checkCmd :: String -> IO ()
checkCmd cmd = do
  ecode <- system cmd
  case ecode of
    ExitSuccess -> return ()
    _ -> fail ("FAILED: " ++ cmd)

test_java :: IO ()
test_java = runTextSlides "test/data/test_java.md" $ \outDir -> do
  let pluginDir = outDir </> "build/plugins/java/"
      referenceDir = "test/data/test_java"
  genFiles <- myListDirectoryRecursive outDir (\p -> ".java" `L.isSuffixOf` p)
  putStrLn (show genFiles)
  let expectedFiles = javaFiles pluginDir
      stripOutDir l =
        flip map l $ \s -> fromMaybe s (L.stripPrefix (outDir ++ "/") s)
      notGenerated = stripOutDir expectedFiles L.\\ stripOutDir genFiles
      extra = stripOutDir genFiles L.\\ stripOutDir expectedFiles
  assertListsEqualAsSetsVerbose
    ("notGenerated=" ++ show notGenerated ++ ", extra=" ++ show extra)
    expectedFiles genFiles
  forM_ expectedFiles $ \f ->
      unless ("test_java.java" `L.isSuffixOf` f) $ do
        putStrLn ("Compiling " ++ f)
        checkCmd ("javac -cp " ++ jarFile ++ " " ++ f)
  let _referenceFiles = javaFiles referenceDir
  _ <- fail "implement test_java"
  return ()
  where
    jarFile = "test/data/junit-platform-console-standalone-1.8.1.jar"
    javaFiles dir =
      let outAll = dir </> "test_java.java"
          outDef1 = dir </> "default_pkg/v01/C1.java"
          outDef2 = dir </> "default_pkg/v02/C1.java"
          outFoo1 = dir </> "foo_v01/foo/Main.java"
          outFoo2 = dir </> "foo_v02/foo/C1.java"
          outFoo3 = dir </> "foo_v03/v01/foo/C2.java"
          outFoo4 = dir </> "foo_v03/v02/foo/C2.java"
          outFoo5 = dir </> "foo_v03/v03/foo/C2.java"
          outFoo6 = dir </> "foo_v03/v04/foo/C2.java"
          outFoo7 = dir </> "foo_v03/v05/foo/C2.java"
          outFoo8 = dir </> "foo_v03/v06/foo/C2.java"
          outFoo9 = dir </> "foo_v03/v07/foo/C2.java"
          outAlt1 = dir </> "alternative/default_pkg_v01/v01/C1.java"
          outAlt2 = dir </> "alternative/default_pkg_v01/v02/C1.java"
          outAlt3 = dir </> "alternative/default_pkg_v02/C3.java"
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
            outFoo9,
            outAlt1,
            outAlt2,
            outAlt3
          ]
