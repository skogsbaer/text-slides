{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.JavaCode
  ( htf_thisModulesTests,
  )
where

import qualified Language.Java.Syntax as JavaSyntax
import Plugins.JavaCode
import Test.Framework

type JLocation = JavaSyntax.Location

mkLoc :: Int -> Int -> JLocation
mkLoc start end = JavaSyntax.Location "<dummy>" start end

test_addVersions :: IO ()
test_addVersions = do
  assertEqual [] (addVersionsIfNecessary [])
  assertEqual ["x"] (addVersionsIfNecessary ["x"])
  assertEqual ["x", "y"] (addVersionsIfNecessary ["x", "y"])
  assertEqual ["x_01", "y", "x_02"] (addVersionsIfNecessary ["x", "y", "x"])

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
      endLoc1 = mkLoc 2 9
  assertEqual "second" (getCode code (mkDecl startLoc1 endLoc1))
  let startLoc2 = mkLoc 2 3
      endLoc2 = mkLoc 3 2
  assertEqual "second line\n}" (getCode code (mkDecl startLoc2 endLoc2))
  where
    code = "first line{\n  second line\n}\nfourth line"

mkDecl :: JLocation -> JLocation -> Decl
mkDecl start end = Decl "foo" start end [] False

test_removeCode :: IO ()
test_removeCode = do
  let startLoc1 = mkLoc 2 3
      endLoc1 = mkLoc 3 2
      decl1 = mkDecl startLoc1 endLoc1
  assertEqual "first line{\n  \nfourth line" (removeCode code [decl1])
  let startLoc2 = mkLoc 1 6
      endLoc2 = mkLoc 2 4
      decl2 = mkDecl startLoc2 endLoc2
  assertEqual "firstecond line\n}\nfourth line" (removeCode code [decl2])
  assertEqual "first\nfourth line" (removeCode code [decl1, decl2])
  assertEqual "first\nfourth line" (removeCode code [decl2, decl1])
  assertEqual
    "ad"
    ( removeCode
        "abcd"
        [mkDecl (mkLoc 1 3) (mkLoc 1 4), mkDecl (mkLoc 1 2) (mkLoc 1 3)]
    )
  assertEqual
    "a"
    ( removeCode
        "abcd"
        [mkDecl (mkLoc 1 3) (mkLoc 1 5), mkDecl (mkLoc 1 2) (mkLoc 1 4)]
    )
  where
    code = "first line{\n  second line\n}\nfourth line"

test_insertCode :: IO ()
test_insertCode = do
  let loc = mkLoc 2 3
  assertEqual "first line{\n  FOOsecond line\n}\nfourth line" (insertCode code loc "FOO")
  where
    code = "first line{\n  second line\n}\nfourth line"

-- decl1                   ^             ^
-- decl2         ^          ^
