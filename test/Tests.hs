{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  -fno-warn-unused-imports #-}

module Main where

import Test.Framework
import Test.Framework.TestManager

{- In Emacs sort block with M-x sort-lines -}
{- In Vim sort block with V}:sort -}
import {-@ HTF_TESTS @-} Test.JavaCode
import {-@ HTF_TESTS @-} Test.Keynote
import {-@ HTF_TESTS @-} Test.Parser
import {-@ HTF_TESTS @-} Test.Plugins
import {-@ HTF_TESTS @-} Test.TestBuildConfig
import {-@ HTF_TESTS @-} Test.Transform
import {-@ HTF_TESTS @-} Test.Utils
import {-@ HTF_TESTS @-} Test.Vars

main :: IO ()
main = htfMain htf_importedTests
