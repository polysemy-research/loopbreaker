-- | To use plugin, you can:
--
-- * Add flag into your project configuration's @ghc-options@ field
-- (@package.yaml@ or @\<name\>.cabal@):
--
-- @
-- # package.yaml
-- ...
--
-- ghc-options:
-- - -fplugin=Loopbreaker
--
-- ...
-- @
--
-- * Add flag to specific modules that may benefit from it's use:
--
-- @
-- -- \<name\>.hs
--
-- {-\# OPTIONS_GHC -fplugin=Loopbreaker \#-}
--
-- ...
-- @
--
-- If you decide to enable it globally, you can selectively disable it in
-- specific modules using @disable@ option:
--
-- @
-- {-\# OPTIONS_GHC -fplugin-opt=Loopbreaker:disable \#-}
--
-- ...
-- @
--
-- Now, in modules where plugin is enabled, loopbreaker will be generated for
-- every self-recursive top-level definition with @INLINE@ pragma, e.g.:
--
-- @
-- fact :: Int -> Int
-- fact 0 = 1
-- fact n = n * fact (n - 1)
-- {-\# INLINE fact \#-}
-- @

-- TODO(Matej): update docs when loopbreakers for local definitions get added

module Loopbreaker (plugin) where

import GhcPlugins

import Loopbreaker.Utils
import Loopbreaker.InlineRecCalls (action)


------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
  { pluginRecompile     = purePlugin
  , renamedResultAction = \opts -> traverse (action opts) .: (,)
  }
