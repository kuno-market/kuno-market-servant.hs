{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Prelude

import Network.KunoMarket.API.Private.Types
import Network.KunoMarket.API.Common.Types
import Test.Hspec
import Test.QuickCheck
import Data.Time
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Scientific (scientific, Scientific)
import Data.List (nubBy, takeWhile, sort)
import Data.Generics.Product (field)
import Lens.Micro ((%~), (&))

main :: IO ()
main =
  hspec $
    describe "Entity JSON round-trip tests" $ do
      it "JSON serialisation for Balance works" $ property $ prop_RoundTrip @Balance
      it "JSON serialisation for BalanceType works" $ property $ prop_RoundTrip @BalanceType
      it "JSON serialisation for WithdrawalRequest works" $ property $ prop_RoundTrip @WithdrawalRequest
      it "JSON serialisation for WithdrawalRequestInformation works" $ property $ prop_RoundTrip @WithdrawalRequestInformation
      it "JSON serialisation for WalletMovement works" $ property $ prop_RoundTrip @WalletMovement
      it "JSON serialisation for FeeStructure works" $ property $ prop_RoundTrip @FeeStructure
      it "JSON serialisation for OrderRequest works" $ property $ prop_RoundTrip @EquableOrderRequest

prop_RoundTrip
  :: (Eq a, FromJSON a, ToJSON a)
  => a -> Bool
prop_RoundTrip t = decode (encode t) == Just t

instance Arbitrary BalanceType where
  arbitrary = elements [Nominal, NominalReserved, Withdrawing]

instance Arbitrary WithdrawalRequest where
  arbitrary = do
    amount <- arbitraryRands
    destination <- arbitrary
    pure WithdrawalRequest {..}

instance Arbitrary WithdrawalRequestInformation where
  arbitrary = do
    amount <- arbitraryBitcoins
    asset <- arbitrary
    destination <- arbitrary
    status <- arbitrary
    timestamp <- arbitrary
    pure WithdrawalRequestInformation {..}

instance Arbitrary Balance where
  arbitrary = do
    balanceType <- arbitrary
    amount <- arbitraryRands
    pure Balance {..}

instance Arbitrary FeeStructure where
  arbitrary = do
    makerFee <- arbitraryPercentage
    takerFee <- arbitraryPercentage
    pure FeeStructure {..}

instance Arbitrary WalletMovement where
  arbitrary = do
    asset <- arbitrary
    amount <- arbitraryBitcoins
    reason <- arbitrary
    timestamp <- arbitrary
    pure WalletMovement {..}

instance Arbitrary EquableOrderRequest where
  arbitrary = do
    size <- arbitraryBitcoins
    side <- arbitrary
    priceType <- arbitrary
    symbol <- arbitrary
    features <- suchThat arbitrary validFeatureList
    pure $ EquableOrderRequest OrderRequest {..}

instance Arbitrary OrderSide where
  arbitrary = elements [Buy, Sell]

instance Arbitrary OrderPriceType where
  arbitrary = do
    price <- arbitraryRands
    elements [Market, Limit price]

instance Arbitrary TimeInForce where
  arbitrary = do
    goodTill <- arbitrary
    elements [GoodTillCancelled, GoodTillTime goodTill, ImmediateOrCancel]

instance Arbitrary PostOnlyType where
  arbitrary = elements [PostOnly, NoPostOnly]

instance Arbitrary TakeProfitType where
  arbitrary = do
    price <- arbitraryRands
    trail <- arbitraryRands
    percentage <- arbitraryPercentage
    elements [NoTakeProfit, VanillaTakeProfit price,
              TrailingTakeProfitNominal trail, TrailingTakeProfitPercentage percentage]

instance Arbitrary StopLossType where
  arbitrary = do
    price <- arbitraryRands
    trail <- arbitraryRands
    percentage <- arbitraryPercentage
    elements [NoStopLoss, VanillaStopLoss price,
              TrailingStopLossNominal trail, TrailingStopLossPercentage percentage]

instance Arbitrary OrderFeature where
  arbitrary = do
    comments <- arbitrary
    stopLoss <- arbitrary
    takeProfit <- arbitrary
    postOnly <- arbitrary
    timeInForce <- arbitrary
    elements [FeatureComments comments, FeatureStopLoss stopLoss, FeatureTakeProfit takeProfit,
             FeaturePostOnly postOnly, FeatureTimeInForce timeInForce]

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

--all of this is necessary because feature lists should be comparable up to permutation
newtype EquableOrderRequest = EquableOrderRequest OrderRequest
  deriving newtype (ToJSON, FromJSON)
  deriving stock Show

instance Eq EquableOrderRequest where
  (EquableOrderRequest a) == (EquableOrderRequest b) =
    (a & (field @"features") %~ sort) == (b & (field @"features") %~ sort)

validFeatureList :: [OrderFeature] -> Bool
validFeatureList ofl = ofl == nubBy featureTypeComparer ofl
  where
    featureTypeComparer ft1 ft2 =
      --Two identical feature types cannot coexist
      takeWhile (/= ' ') (show ft1) == takeWhile (/= ' ') (show ft2)

arbitraryBitcoins :: Gen Scientific
arbitraryBitcoins = do
  satoshis <- choose (1, 1e11)
  pure $ scientific satoshis (negate 8)

arbitraryRands :: Gen Scientific
arbitraryRands = do
  cents <- choose (1, 1e8)
  pure $ scientific cents (negate 2)

arbitraryPercentage :: Gen Scientific
arbitraryPercentage = do
  percentage <- choose (0, 1e4)
  pure $ scientific percentage (negate 4)
