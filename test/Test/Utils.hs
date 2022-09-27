{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Utils
  ( htf_thisModulesTests,
  )
where

import Test.Framework
import Utils

test_subText :: IO ()
test_subText = do
  assertEqual (subText 1 "Hello" 3) "el"
  assertEqual (subText 1 "Hello" 4) "ell"
  assertEqual (subText 1 "Hello" 5) "ello"
  assertEqual (subText 1 "Hello" 6) "ello"
  assertEqual (subText 1 "Hello" 0) ""

test_replaceText :: IO ()
test_replaceText = do
  assertEqual (replaceText 1 "Hello" 3 "123") "H123lo"
  assertEqual (replaceText 1 "Hello" 5 "123") "H123"
  assertEqual (replaceText 1 "Hello" 6 "123") "H123"
  assertEqual (replaceText 1 "Hello" 1 "123") "H123ello"
  assertEqual (replaceText 1 "Hello" 0 "123") "H123ello"

test_insertText :: IO ()
test_insertText = assertEqual (insertText 1 "Hello" "123") "H123ello"

test_deleteText :: IO ()
test_deleteText = do
  assertEqual (deleteText 1 "Hello" 1) "Hello"
  assertEqual (deleteText 1 "Hello" 0) "Hello"
  assertEqual (deleteText 1 "Hello" 2) "Hllo"
  assertEqual (deleteText 1 "Hello" 3) "Hlo"
  assertEqual (deleteText 1 "Hello" 6) "H"
