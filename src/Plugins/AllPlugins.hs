module Plugins.AllPlugins where

import qualified Data.Map.Strict as M
import Types
import Development.Shake
import Plugins.Keynote
import Plugins.Mermaid
import Plugins.Code

allPlugins :: [ExternalLangConfig] -> [AnyPluginConfig Action]
allPlugins moreLanguages = [keynotePlugin, mermaidPlugin] ++ (codePlugins moreLanguages)

pluginMap :: BuildConfig -> PluginMap Action
pluginMap cfg =
  M.fromList $
    map
      (\(AnyPluginConfig p) -> (p_name p, AnyPluginConfig p))
      (allPlugins (unExternalLangConfigs  (bc_externalLangConfigs cfg)))

allPluginRules :: BuildArgs -> Rules ()
allPluginRules args = do
  keynotePluginRules args
  mermaidPluginRules args
