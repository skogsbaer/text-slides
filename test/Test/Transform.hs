{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Transform
  ( htf_thisModulesTests,
  )
where

import CoreRules
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Test.Framework
import Test.TestBuildConfig
import Text.Heredoc
import Types

test_transform :: IO ()
test_transform = do
  (out, _) <-
    transformMarkdown
      (\_ -> return ())
      testBuildConfig
      testBuildArgs
      pluginMap
      "<input>"
      sampleInput
  assertEqual expected out
  where
    keynote :: AnyPluginConfig IO
    keynote =
      AnyPluginConfig $
        PluginConfig
          { p_name = PluginName "keynote",
            p_kind = PluginWithoutBody,
            p_init = return (),
            p_expand = \_cfg _args () call -> do
              file <- exceptInM $ getRequiredStringValue unknownLocation "file" (pc_args call)
              slide <- exceptInM $ getRequiredIntValue unknownLocation "slide" (pc_args call)
              return
                ( "![](build/plugins/keynote/"
                    <> file
                    <> "/"
                    <> T.pack (show slide)
                    <> ".jpg)",
                  ()
                ),
            p_forAllCalls = \_cfg _args _ -> return ()
          }
    python :: AnyPluginConfig IO
    python =
      AnyPluginConfig $
        PluginConfig
          { p_name = PluginName "python",
            p_kind = PluginWithBody,
            p_init = return (),
            p_expand = \_cfg _args () call -> return ("~~~python\n" <> pc_body call <> "\n~~~", ()),
            p_forAllCalls = \_cfg _args _ -> return ()
          }
    pluginMap = M.fromList (map (\p -> (anyPluginName p, p)) [keynote, python])

sampleInput :: T.Text
sampleInput =
  [here|
~~~keynote(file: "my_presentation.key", slide: 1)

-- Source code --
~~~python(file: "foo.py")
print(foo(41))
~~~

~~~foo
|]

expected :: T.Text
expected =
  [here|
![](build/plugins/keynote/my_presentation.key/1.jpg)

-- Source code --
~~~python
print(foo(41))
~~~

~~~foo
|]
