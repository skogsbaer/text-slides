{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Keynote where

import Plugins.Keynote
import System.Directory
import System.FilePath
import Test.Framework
import Test.TestBuildConfig

test_keynoteFileToBuildPath :: IO ()
test_keynoteFileToBuildPath = do
  subAssert $
    check "src/my_presi.key" "build/plugins/keynote/rel/src/my_presi.key" "src/my_presi.key"
  subAssert $
    check "./src/my_presi.key" "build/plugins/keynote/rel/src/my_presi.key" "src/my_presi.key"
  subAssert $
    check
      "/home/swehr/my_presi.key"
      "build/plugins/keynote/abs/home/swehr/my_presi.key"
      "/home/swehr/my_presi.key"
  curDir <- getCurrentDirectory
  subAssert $
    check
      "../my_presi.key"
      ("build/plugins/keynote/abs" ++ (takeDirectory curDir) </> "my_presi.key")
      (takeDirectory curDir </> "my_presi.key")
  where
    cfg = testBuildConfig []
    check fp expected expectedBack = do
      build <- keynoteFileToBuildPath cfg fp
      assertEqual expected build
      assertEqual (Right expectedBack) (buildPathToKeynoteFile cfg build)

test_buildPathToKeynoteFile :: IO ()
test_buildPathToKeynoteFile = do
  assertEqual
    (Right "src/my_presi.key")
    (buildPathToKeynoteFile cfg "build/plugins/keynote/rel/src/my_presi.key")
  assertEqual
    (Right "src/my_presi.key")
    (buildPathToKeynoteFile cfg "./build/plugins/keynote/rel/src/my_presi.key")
  assertEqual
    (Right "/home/stefan/presi.key")
    (buildPathToKeynoteFile cfg "./build/plugins/keynote/abs/home/stefan/presi.key")
  where
    cfg = testBuildConfig []
