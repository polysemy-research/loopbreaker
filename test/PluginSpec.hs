{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE TemplateHaskell #-}

module PluginSpec (spec) where

import Test.Hspec
import Test.Inspection


------------------------------------------------------------------------------
-- TODO: more tests
spec :: Spec
spec = describe "plugin" $ do
  it "should explicitly break recursion" $ do
    shouldSucceed $(inspectTest $ 'recursive === 'mutual)

------------------------------------------------------------------------------
recursive :: Int -> Int
recursive 0 = 1
recursive n = n * recursive (n - 1)

mutual :: Int -> Int
mutual 0 = 1
mutual n = n * mutual' (n - 1)
{-# INLINE mutual #-}

mutual' :: Int -> Int
mutual' = mutual
{-# NOINLINE mutual' #-}

------------------------------------------------------------------------------
shouldSucceed :: Result -> Expectation
shouldSucceed r = r `shouldSatisfy` \case Success{} -> True
                                          Failure e -> error e
