{-# LANGUAGE DataKinds #-}

module Main where

import Amount
import Data.Ratio
import Test.Tasty.Bench

main :: IO ()
main =
    defaultMain
        [ bgroup
            "Amount"
            [ bench "show Amount 2" $ nf show (Amount (355 % 113) :: Amount 2)
            , bench "show Amount 8" $ nf show (Amount (355 % 113) :: Amount 8)
            , bench "arithmetic" $
                nf
                    (\x -> x + x * 2 - x)
                    (Amount (355 % 113) :: Amount 6)
            , bench "equality" $
                nf
                    (\x -> x == Amount (fromRational (355 % 113)))
                    (Amount (355 % 113) :: Amount 6)
            , bench "spreadAmounts" $
                nf
                    (spreadAmounts id (100 :: Amount 2))
                    (replicate 7 (1 :: Amount 2))
            ]
        ]
