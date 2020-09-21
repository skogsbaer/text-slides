{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Parser
  ( htf_thisModulesTests,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Parser
import Test.Framework
import Types

test_parsePluginCall :: IO ()
test_parsePluginCall = do
  check "~~~foo  ()" (PluginCall (PluginName "foo") loc Nothing M.empty "")
  check "~~~foo  " (PluginCall (PluginName "foo") loc Nothing M.empty "")
  check
    "~~~foo  (  k1 : \"string\",k2:42  , k3: true )  "
    ( PluginCall
        { pc_pluginName = PluginName "foo",
          pc_location = loc,
          pc_args =
            M.fromList
              [ ("k1", ArgString "string"),
                ("k2", ArgInt 42),
                ("k3", ArgBool True)
              ],
          pc_body = "",
          pc_sectionName = Nothing
        }
    )
  where
    loc = Location "<input>"
    check input expected =
      assertEqual (Right expected) (parsePluginCall "<input>" Nothing input)

test_parse :: IO ()
test_parse = do
  let tokens = parseMarkdown "<input>" (M.fromList plugins) sampleInput
  assertEqual (Right (expected "5")) tokens
  let tokens2 = parseMarkdown "<input>" (M.fromList plugins) sampleInput2
  assertEqual (Right (expected "5")) tokens2
  let tokens3 = parseMarkdown "<input>" (M.fromList plugins) sampleInput3
  assertEqual (Right (expected "6")) tokens3
  where
    plugins =
      [ (PluginName "keynote", PluginWithoutBody),
        (PluginName "python", PluginWithBody)
      ]
    expected pyPluginLineNo =
      [ Line "",
        Plugin $
          PluginCall
            { pc_pluginName = (PluginName "keynote"),
              pc_location = (Location "<input>:2"),
              pc_args =
                M.fromList
                  [ ("file", ArgString "my_presentation.key"),
                    ("slide", ArgInt 1)
                  ],
              pc_sectionName = Nothing,
              pc_body = ""
            },
        Line "",
        Line "## Source code",
        Plugin $
          PluginCall
            { pc_pluginName = PluginName "python",
              pc_location = Location ("<input>:" <> pyPluginLineNo),
              pc_args = M.empty,
              pc_body = "print(foo(41))",
              pc_sectionName = Just "Source code"
            },
        Line "",
        Line "~~~foo"
      ]

sampleInput :: T.Text
sampleInput =
  T.unlines
    [ "",
      "~~~keynote(file: \"my_presentation.key\", slide: 1)",
      "",
      "## Source code",
      "~~~python",
      "print(foo(41))",
      "~~~",
      "",
      "~~~foo"
    ]

sampleInput2 :: T.Text
sampleInput2 =
  T.unlines
    [ "",
      "~~~keynote (file: \"my_presentation.key\", slide: 1) ~~~",
      "",
      "## Source code",
      "~~~python",
      "print(foo(41))",
      "~~~",
      "",
      "~~~foo"
    ]

sampleInput3 :: T.Text
sampleInput3 =
  T.unlines
    [ "",
      "~~~keynote(file: \"my_presentation.key\", slide: 1)",
      "~~~",
      "",
      "## Source code",
      "~~~python",
      "print(foo(41))",
      "~~~",
      "",
      "~~~foo"
    ]
