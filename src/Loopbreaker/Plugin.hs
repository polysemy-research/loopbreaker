-- TODO: write description

module Loopbreaker.Plugin (plugin) where

import GhcPlugins

import Loopbreaker.Utils
import Loopbreaker.InlineRecCalls (action)


------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
  { pluginRecompile     = purePlugin
  , renamedResultAction = \opts -> traverse (action opts) .: (,)
  }
