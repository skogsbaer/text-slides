{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.TestBuildConfig
  ( testBuildConfig,
    testBuildArgs,
    htf_thisModulesTests,
  )
where

import BuildConfig
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Test.Framework
import Text.Heredoc
import Types

testBuildConfig :: BuildConfig
testBuildConfig =
  BuildConfig
    { bc_pandoc = "pandoc",
      bc_pdflatex = "pdflatex",
      bc_syntaxTheme = Nothing,
      bc_python = "python3",
      bc_convert = "convert",
      bc_mermaid = "mermaid",
      bc_beamerHeader = [],
      bc_htmlHeader = Nothing,
      bc_luaFilter = Nothing,
      bc_externalLangConfigs = ExternalLangConfigs [],
      bc_pdfcrop = "pdfcrop",
      bc_mermaidConfig = Nothing
    }

testBuildArgs :: BuildArgs
testBuildArgs =
  BuildArgs
    { ba_inputFile = "sample.md",
      ba_verbose = False,
      ba_searchDir = "."
    }

test_parseExternalLangConfig :: IO ()
test_parseExternalLangConfig = do
  assertEqual (Right expected) (J.eitherDecode' (BSL.fromStrict $ T.encodeUtf8 sampleInput))
  where
    expected =
      ExternalLangConfigs
        [ ExternalLangConfig
            { elc_name = "python-repl",
              elc_fileExt = ".py",
              elc_commentStart = "#",
              elc_commentEnd = Nothing,
              elc_syntaxFile = Just "syntax/python-repl.xml"
            }
        ]

sampleInput :: T.Text
sampleInput =
  [here|
{ "languages": [
    { "name": "python-repl",
      "syntaxFile": "syntax/python-repl.xml",
      "extension": ".py",
      "commentStart": "#"
    }
  ]
}
|]
