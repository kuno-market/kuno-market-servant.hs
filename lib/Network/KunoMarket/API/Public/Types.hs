{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Network.KunoMarket.API.Public.Types
  ( Trades
  , Trade(..)
  , Ticker(..)
  , OrderSide(..)
  , PriceLevel(..)
  , OrderBook(..)
  ) where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Data (Data)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), Value(..), (.:), (.=), ToJSON(..), pairs)
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Text (toUpper, unpack)
import Data.Ratio ((%))
import Text.Read (readEither)
import Data.Scientific
import Data.Monoid ((<>))

type Trades = [Trade]

data Trade = Trade
  { volume    :: Scientific
  , price     :: Scientific
  , side      :: OrderSide
  , timestamp :: UTCTime
} deriving (Show, Eq, Ord, Data, Generic, NFData)

data Ticker = Ticker
  { buy  :: Maybe Scientific
  , sell :: Maybe Scientific
  , last :: Maybe Scientific
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data OrderSide = Buy | Sell
  deriving (Show, Eq, Ord, Data, Generic, NFData, ToJSON)

data OrderBook = OrderBook
 { sells :: [PriceLevel]
 , buys  :: [PriceLevel]
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data PriceLevel = PriceLevel
  { volume :: Scientific
  , price  :: Scientific
} deriving (Show, Eq, Ord, Data, Generic, NFData)

--------------------------------------------------------------------------------

instance FromJSON OrderSide where
  parseJSON (String s) =
    case toUpper s of
      "BUY"  -> pure Buy
      "SELL" -> pure Sell
      _ -> fail $ "Unknown order-side type: " ++ show s
  parseJSON s = typeMismatch "OrderSide" s

instance FromJSON Trade where
  parseJSON (Object v) = Trade
    <$> (v .: "volume" >>= parseQuotedScientific)
    <*> (v .: "price"  >>= parseQuotedScientific)
    <*>  v .: "side"
    <*> fmap (posixSecondsToUTCTime . fromRational . (% 1e3)) (v .: "timestamp")
  parseJSON v = typeMismatch "Trade" v

instance FromJSON PriceLevel where
  parseJSON (Object v) = PriceLevel
    <$> (v .: "volume" >>= parseQuotedScientific)
    <*> (v .: "price"  >>= parseQuotedScientific)
  parseJSON v = typeMismatch "PriceLevel" v

parseQuotedScientific :: Value -> Parser Scientific
parseQuotedScientific (String q) =
  case readEither @Scientific $ unpack q of
    Left _  -> fail "failure parsing quoted decimal"
    Right d -> pure d
parseQuotedScientific (Number q) = pure q
parseQuotedScientific v = typeMismatch "Number" v

--------------------------------------------------------------------------------

instance ToJSON Trade where
  toEncoding Trade {..} =
    pairs (
      "volume"    .= show volume <>
      "price"     .= show price <>
      "side"      .= side <>
      "timestamp" .= showUTCTime timestamp
    )

instance ToJSON PriceLevel where
  toEncoding PriceLevel {..} =
    pairs (
      "volume" .= show volume <>
      "price"  .= show price
    )

showUTCTime :: UTCTime -> Int
showUTCTime t = round @Double $ (* 1e3) $ realToFrac $ utcTimeToPOSIXSeconds t
