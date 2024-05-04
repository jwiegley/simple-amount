{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Amount
import Data.Ratio
import Hedgehog
import Control.Monad.IO.Class
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main =
  defaultMain $
    testGroup
      "simple-amount"
      [ testProperty "basic properties" prop_basic
      -- , testProperty "basic math" prop_math
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

{-
prop_math :: Property
prop_math = property $ do
  let x :: Amount 100 = read "15.034465284692086701747761395233132973944448512421004399685858401206740385711739229018307610943234609057822959334669087436253689423614206061665462283698768757790600552385430913941421707844383369633809803959413869974997415115322843838226312287673293352959835"
  let y :: Amount 100 = read "3.466120406090666777582519661568003549307295836842780244500133445635634490670936927006970368136648330889718447039413255137656971927890831071689768359173260960739254160211017410322799793419223796996260056081828170546988461285168124170297427792046640116184356"
  let z :: Amount 100 = read "12020.67042599064370733685791492462158203125"
  liftIO $ assertEqual "div" (x ** y) z
-}
