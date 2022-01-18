{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Vars
  ( htf_thisModulesTests,
  )
where

import qualified Data.Map as M
import Test.Framework
import Vars

test_expandVars :: IO ()
test_expandVars = do
  let t = expandVars varMap "Hello {{name}}, how are you {{{{today}}? Unknown {{variable}} {{x{{name}}!"
  assertEqual "Hello Stefan, how are you {{2022-01-18? Unknown {{variable}} {{xStefan!" t
  let t2 = "{{x{{y{{z}}}}}}"
  assertEqual t2 (expandVars varMap t2)
  where
    varMap = VarMap (M.fromList [("name", "Stefan"), ("today", "2022-01-18")])

test_readVars :: IO ()
test_readVars = do
  vm <- readVarsFromBs  "<input>" "lecture: \"Anwendungsentwicklung\"\npingoSession: \"230050\""
  let l = [("lecture", "Anwendungsentwicklung"), ("pingoSession", "230050")]
  assertEqual (VarMap (M.fromList l)) vm
