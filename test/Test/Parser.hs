{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Parser where

import qualified Data.Map as M
import qualified Data.Text as T
import Parser
import Test.Framework
import Text.Heredoc
import Types

test_parsePluginCall :: IO ()
test_parsePluginCall = do
  check "~~~foo  ()" (PluginCall (PluginName "foo") loc M.empty "")
  check "~~~foo  " (PluginCall (PluginName "foo") loc M.empty "")
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
          pc_body = ""
        }
    )
  where
    loc = Location "<input>"
    check input expected =
      assertEqual (Right expected) (parsePluginCall "<input>" input)

test_parse :: IO ()
test_parse = do
  let tokens = parseMarkdown "<input>" (M.fromList plugins) sampleInput
  assertEqual (Right expected) tokens
  where
    plugins =
      [ (PluginName "keynote", PluginWithoutBody),
        (PluginName "python", PluginWithBody)
      ]
    expected =
      [ Line "",
        Plugin $
          PluginCall
            (PluginName "keynote")
            (Location "<input>:2")
            ( M.fromList
                [ ("file", ArgString "my_presentation.key"),
                  ("slide", ArgInt 1)
                ]
            )
            "",
        Line "",
        Line "-- Source code --",
        Plugin $ PluginCall (PluginName "python") (Location "<input>:5") M.empty "print(foo(41))",
        Line "",
        Line "~~~foo"
      ]

sampleInput :: T.Text
sampleInput =
  [here|
~~~keynote(file: "my_presentation.key", slide: 1)

-- Source code --
~~~python
print(foo(41))
~~~

~~~foo
|]
