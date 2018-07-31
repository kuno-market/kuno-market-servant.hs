{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Prelude

import Network.KunoMarket.API.Types
import Test.Hspec
import Test.QuickCheck
import Data.Time
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Scientific (scientific, Scientific)

main :: IO ()
main =
  hspec $
    describe "Entity JSON round-trip tests" $ do
      it "JSON serialisation for Trade works"      $ property $ prop_RoundTrip @Trade
      it "JSON serialisation for Ticker works"     $ property $ prop_RoundTrip @Ticker
      it "JSON serialisation for PriceLevel works" $ property $ prop_RoundTrip @PriceLevel

prop_RoundTrip
  :: (Eq a, FromJSON a, ToJSON a)
  => a -> Bool
prop_RoundTrip t = decode (encode t) == Just t

instance Arbitrary Trade where
  arbitrary = do
    volume <- arbitraryBitcoins
    price <- arbitraryRands
    side <- elements [Buy, Sell]
    timestamp <- arbitrary
    pure Trade {..}

instance Arbitrary PriceLevel where
  arbitrary = do
    volume <- arbitraryBitcoins
    price <- arbitraryRands
    pure PriceLevel {..}

instance Arbitrary Ticker where
  arbitrary = do
    b <- arbitraryRands
    buy <- elements [Nothing, Just b, Just b]
    s <- arbitraryRands
    sell <- elements [Nothing, Just s, Just s]
    l <- arbitraryRands
    last <- elements [Nothing, Just l, Just l]
    pure Ticker {..}

instance Arbitrary UTCTime where
  arbitrary = do
    d <- choose (1, 29)
    m <- choose (1, 12)
    y <- choose (1984, 2018)
    t <- choose (0, 86401)
    return $
      UTCTime
        (fromGregorian y m d)
        (fromInteger t)

arbitraryBitcoins :: Gen Scientific
arbitraryBitcoins = do
  satoshis <- choose (1, 1e11)
  pure $ scientific satoshis (negate 8)

arbitraryRands :: Gen Scientific
arbitraryRands = do
  cents <- choose (1, 1e8)
  pure $ scientific cents (negate 2)
