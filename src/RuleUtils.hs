module RuleUtils where

import qualified Data.List as L
import System.FilePath
import Types

outputFileToInputFile :: BuildConfig -> BuildArgs -> FilePath -> FilePath
outputFileToInputFile cfg args out =
  let prefix = bc_buildDir cfg ++ "/"
   in if not (prefix `L.isPrefixOf` out)
        then error ("outputFileToInputFile: not an output file: " ++ out)
        else ba_searchDir args </> drop (length prefix) out
