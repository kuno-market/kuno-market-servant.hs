{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?))
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Text (toUpper, unpack)
import Data.Ratio ((%))
import Text.Read (readEither)

type Trades = [Trade]

data Trade = Trade
  { volume    :: Rational
  , price     :: Rational
  , side      :: OrderSide
  , timestamp :: UTCTime
} deriving (Show, Eq, Ord, Data, Generic, NFData)

data Ticker = Ticker
  { buy  :: Maybe Rational
  , sell :: Maybe Rational
  , last :: Maybe Rational
} deriving (Show, Eq, Ord, Data, Generic, NFData)

data OrderSide = Buy | Sell
  deriving (Show, Eq, Ord, Data, Generic, NFData)

data OrderBook = OrderBook
 { sells :: [PriceLevel]
 , buys  :: [PriceLevel]
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON)

data PriceLevel = PriceLevel
  { volume :: Rational
  , price  :: Rational
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
    <$> (v .: "volume" >>= parseQuotedDecimal)
    <*> (v .: "price"  >>= parseQuotedDecimal)
    <*>  v .: "side"
    <*> fmap (posixSecondsToUTCTime . fromRational . (% 1000)) (v .: "timestamp")
  parseJSON v = typeMismatch "Trade" v

instance FromJSON Ticker where
  parseJSON (Object v) = Ticker
    <$> (v .:? "buy"  >>= traverse parseQuotedDecimal)
    <*> (v .:? "sell" >>= traverse parseQuotedDecimal)
    <*> (v .:? "last" >>= traverse parseQuotedDecimal)
  parseJSON v = typeMismatch "Ticker" v

instance FromJSON PriceLevel where
  parseJSON (Object v) = PriceLevel
    <$> (v .: "volume" >>= parseQuotedDecimal)
    <*> (v .: "price"  >>= parseQuotedDecimal)
  parseJSON v = typeMismatch "PriceLevel" v

parseQuotedDecimal :: Value -> Parser Rational
parseQuotedDecimal (String q) =
  case readEither @Double $ unpack q of
    Left _  -> fail "failure parsing quoted decimal"
    Right d -> pure $ toRational d
parseQuotedDecimal (Number q) = pure $ toRational q
parseQuotedDecimal v = typeMismatch "Number" v
