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

module Network.KunoMarket.API.Types
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

type Trades = [Trade]

data Trade = Trade
  { volume    :: Scientific
  , price     :: Scientific
  , side      :: OrderSide
  , timestamp :: UTCTime
} deriving (Show, Eq, Ord, Data, Generic, NFData)

data Ticker = Ticker
} deriving (Show, Eq, Ord, Data, Generic, NFData)
  { buy  :: Maybe Scientific
  , sell :: Maybe Scientific
  , last :: Maybe Scientific

data OrderSide = Buy | Sell
  deriving (Show, Eq, Ord, Data, Generic, NFData)

data OrderBook = OrderBook
 { sells :: [PriceLevel]
 , buys  :: [PriceLevel]
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON)

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
