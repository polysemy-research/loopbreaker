-- TODO: write description

module Loopbreaker.Plugin (plugin) where

import GhcPlugins

import Loopbreaker.Utils
import Loopbreaker.InlineRecCalls.CoreToDos
import Loopbreaker.InlineRecCalls.Action


------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
    -- TODO: this is only useful from 8.10?
  { installCoreToDos    = const installToDos
  , pluginRecompile     = purePlugin
  , renamedResultAction = const $ traverse action .: (,)
  }
