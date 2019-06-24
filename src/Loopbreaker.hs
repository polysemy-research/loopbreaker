{-# LANGUAGE DeriveDataTypeable #-}

-- TODO: write documentation

module Loopbreaker
  ( LoopbreakerAnn (..)
  ) where

import Data.Data
import Outputable


------------------------------------------------------------------------------
-- TODO: write documentation
data LoopbreakerAnn = Loopbreaker | NoLoopbreaker deriving (Data, Show, Eq)

instance Outputable LoopbreakerAnn where
  ppr = text . show
