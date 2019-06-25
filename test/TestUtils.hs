module TestUtils
  ( module U
  , shouldSucceed
  , shouldFail
  ) where

import Test.Hspec      as U
import Test.Inspection as U


------------------------------------------------------------------------------
shouldSucceed :: Result -> Expectation
shouldSucceed r = r `shouldSatisfy` \case Success{} -> True
                                          Failure e -> error e

shouldFail :: Result -> Expectation
shouldFail r = r `shouldSatisfy` \case Success m -> error m
                                       Failure{} -> True
