{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Amount
import Data.Ratio
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main =
  defaultMain $
    testGroup
      "simple-amount"
      [ testProperty "basic properties" prop_basic
      ]

prop_basic :: Property
prop_basic = property $ do
  x <-
    forAll $
      (%) <$> Gen.integral (Range.linear 0 1_000_000_000)
        <*> Gen.integral (Range.linear 1 1_000_000_000)
  y <-
    forAll $
      (%) <$> Gen.integral (Range.linear 0 1_000_000_000)
        <*> Gen.integral (Range.linear 1 1_000_000_000)
  diff ((Amount x :: Amount 6) + Amount y) (>=) 0
