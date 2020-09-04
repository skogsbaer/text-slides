{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Transform where

import CoreRules
import Types
import Text.Heredoc
import Test.Framework
import qualified Data.Text as T

test_transform :: IO ()
test_transform = do
    out <- transformMarkdown (\_ -> return ()) plugins "<input>" sampleInput
    assertEqual expected out
  where
    keynote =
        PluginConfig
        { p_name = PluginName "keynote"
        , p_kind = PluginWithoutBody
        , p_rules = return ()
        , p_expand = \call -> do
            file <- exceptInM $ getRequiredStringValue "file" (pc_args call)
            slide <- exceptInM $ getRequiredIntValue "slide" (pc_args call)
            return $ "![](build/plugins/keynote/" <>
                file <> "/" <> T.pack (show slide) <> ".jpg)"
        }
    python =
        PluginConfig
        { p_name = PluginName "python"
        , p_kind = PluginWithBody
        , p_rules = return ()
        , p_expand = \call -> return $ "~~~python\n" <> pc_body call <> "\n~~~"
        }
    plugins = [keynote, python]

sampleInput :: T.Text
sampleInput = [here|
~~~keynote(file: "my_presentation.key", slide: 1)

-- Source code --
~~~python(file: "foo.py")
print(foo(41))
~~~

~~~foo
|]

expected :: T.Text
expected = [here|
![](build/plugins/keynote/my_presentation.key/1.jpg)

-- Source code --
~~~python
print(foo(41))
~~~

~~~foo
|]
