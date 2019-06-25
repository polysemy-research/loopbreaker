{-# OPTIONS_GHC -O2 -fplugin-opt=Loopbreaker.Plugin:disable #-}

{-# LANGUAGE TemplateHaskell #-}

module DisableFlagSpec (spec) where

import TestUtils


------------------------------------------------------------------------------
spec :: Spec
spec = describe "plugin" $ do
  it "should respect disable flag" $ do
    shouldFail $(inspectTest $ 'recursiveDefault === 'mutual)
    shouldFail $(inspectTest $ 'recursiveOn      === 'mutual)

------------------------------------------------------------------------------
recursiveDefault :: Int -> Int
recursiveDefault 0 = 1
recursiveDefault n = n * recursiveDefault (n - 1)

recursiveOn :: Int -> Int
recursiveOn 0 = 1
recursiveOn n = n * recursiveOn (n - 1)
{-# INLINE recursiveOn #-}

mutual :: Int -> Int
mutual 0 = 1
mutual n = n * mutual' (n - 1)
{-# INLINE mutual #-}

mutual' :: Int -> Int
mutual' = mutual
{-# NOINLINE mutual' #-}
