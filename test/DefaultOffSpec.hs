{-# OPTIONS_GHC -O2 -fplugin-opt=Loopbreaker.Plugin:default_off #-}

{-# LANGUAGE TemplateHaskell #-}

module DefaultOffSpec (spec) where

import TestUtils


------------------------------------------------------------------------------
spec :: Spec
spec = describe "plugin" $ do
  it "should respect default_off flag" $ do
    shouldFail    $(inspectTest $ 'recursiveDefault === 'mutual)
    shouldSucceed $(inspectTest $ 'recursiveOn      === 'mutual)
    shouldFail    $(inspectTest $ 'recursiveOff     === 'mutual)

------------------------------------------------------------------------------
recursiveDefault :: Int -> Int
recursiveDefault 0 = 1
recursiveDefault n = n * recursiveDefault (n - 1)

recursiveOn:: Int -> Int
recursiveOn 0 = 1
recursiveOn n = n * recursiveOn (n - 1)
{-# ANN recursiveOn Loopbreaker #-}

recursiveOff :: Int -> Int
recursiveOff 0 = 1
recursiveOff n = n * recursiveOff (n - 1)
{-# ANN recursiveOff NoLoopbreaker #-}

mutual :: Int -> Int
mutual 0 = 1
mutual n = n * mutual' (n - 1)
{-# INLINE mutual #-}

mutual' :: Int -> Int
mutual' = mutual
{-# NOINLINE mutual' #-}
