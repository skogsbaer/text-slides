module RuleUtils where

import qualified Data.List as L
import System.FilePath
import Types

outputFileToInputFile :: BuildConfig -> FilePath -> FilePath
outputFileToInputFile cfg out =
  let prefix = bc_buildDir cfg ++ "/"
   in if not (prefix `L.isPrefixOf` out)
        then error ("outputFileToInputFile: not an output file: " ++ out)
        else bc_searchDir cfg </> drop (length prefix) out
