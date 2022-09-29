{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.JavaCode
  ( htf_thisModulesTests,
  )
where

import qualified Language.Java.Syntax as JavaSyntax
import Plugins.JavaCode
import Test.Framework
import qualified Data.List as L
import Control.Monad

type JLocation = JavaSyntax.Location

mkLoc :: Int -> Int -> JLocation
mkLoc start end = JavaSyntax.Location "<dummy>" start end

test_addVersions :: IO ()
test_addVersions = do
  assertEqual [] (addVersionsIfNecessary [])
  assertEqual ["x"] (addVersionsIfNecessary ["x"])
  assertEqual ["x", "y"] (addVersionsIfNecessary ["x", "y"])
  assertEqual ["x_v01", "y", "x_v02"] (addVersionsIfNecessary ["x", "y", "x"])

test_locationToIndex :: IO ()
test_locationToIndex = do
  assertEqual 0 (locationToIndex code (mkLoc 1 1))
  assertEqual 12 (locationToIndex code (mkLoc 2 1))
  assertEqual 14 (locationToIndex code (mkLoc 2 3))
  where
    code = "first line{\n  second line\n}\nfourth line"

test_locationToIndex2 :: IO ()
test_locationToIndex2 = do
  assertEqual 0 (locationToIndex code (mkLoc 1 1))
  assertEqual 5 (locationToIndex code (mkLoc 2 1))
  assertEqual 7 (locationToIndex code (mkLoc 2 3))
  where
    code = "123\r\nxyz\r\n"

test_getCode :: IO ()
test_getCode = do
  let startLoc1 = mkLoc 2 3
      endLoc1 = mkLoc 2 8
  assertEqual "second" (getCode code (mkDecl startLoc1 endLoc1))
  let startLoc2 = mkLoc 2 3
      endLoc2 = mkLoc 3 1
  assertEqual "second line\n}" (getCode code (mkDecl startLoc2 endLoc2))
  where
    code = "first line{\n  second line\n}\nfourth line"
-- loc1                    ^    ^
-- loc2                    ^               ^

mkDecl :: JLocation -> JLocation -> Decl
mkDecl start end = Decl "foo" start end [] False

test_patchCode :: IO ()
test_patchCode = do
  let patches =
        [ CodePatchInsert 3 "r",
          CodePatchReplace 1 "eathe" 2,
          CodePatchInsert 0 "A ",
          CodePatchReplace 4 "is" 6,
          CodePatchAppend " baz"
        ]
  forM_ (L.permutations patches) $ \ps ->
    assertEqualVerbose ("ps=" ++ show ps) newCode (patchCode code ps)
  where
    code = "foo bar"
    newCode = "A feather is baz"
