{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Network.KunoMarket.API.Public.Types
  ( Trade(..)
  , Ticker(..)
  , OrderSide(..)
  , PriceLevel(..)
  , OrderBook(..)
  ) where

import Data.Time
import Data.Data (Data)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), Value(..), (.:), (.=), ToJSON(..), pairs)
import Data.Aeson.Types (typeMismatch)
import Data.Scientific
import Data.Monoid ((<>))
import Network.KunoMarket.API.Common.Types (OrderSide(..), parseQuotedScientific,
                                           utcTimeToMS, msToUTCTime)

-- | Each Trade is presented from the viewpoint of the __maker__ order (already in the order-book).
--
-- For example, if we had a limit buy order at some price on the order book, and it matched against
--  an incoming market sell order, then the 'Trade' object would signify how many units of the quote
--  currency the maker had to pay the taker. On the other side, the taker would give some amount of
--  the base currency to the maker, but there would be no 'Trade' object denoting this reciprocal event.
data Trade = Trade
  { amount    :: Scientific
    -- ^ For Buy order, as units of Quote currency (e.g. ZAR) given.
    --   For Sell order, as units of Base currency (e.g. BTC) given.
  , price     :: Scientific -- ^ Price at which this order happened.
  , side      :: OrderSide -- ^ Side of the maker order which was already on the book.
  , timestamp :: UTCTime
} deriving (Show, Eq, Ord, Data, Generic, NFData)

data Ticker = Ticker
  { buy  :: Maybe Scientific
    -- ^ Will be 'Nothing' if there are no buy orders,
    --   otherwise it be the best (highest) price of any buy orders in the order book.
  , sell :: Maybe Scientific
    -- ^ Will be 'Nothing' if there are no sell orders.
    --   otherwise it be the best (lowest) price of any sell orders in the order book.
  , last :: Maybe Scientific -- ^ Will be 'Nothing' if the market has yet to see any trades.
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

-- | Note that orders are conflated in each price level, thus there is only a single volume
--   per price.
data OrderBook = OrderBook
 { sells :: [PriceLevel]
 , buys  :: [PriceLevel]
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data PriceLevel = PriceLevel
  { volume :: Scientific -- ^ In base currency units.
  , price  :: Scientific
} deriving (Show, Eq, Ord, Data, Generic, NFData)

--------------------------------------------------------------------------------

instance FromJSON Trade where
  parseJSON (Object v) = Trade
    <$> (v .: "amount" >>= parseQuotedScientific)
    <*> (v .: "price"  >>= parseQuotedScientific)
    <*>  v .: "side"
    <*> fmap msToUTCTime (v .: "timestamp")
  parseJSON v = typeMismatch "Trade" v

instance FromJSON PriceLevel where
  parseJSON (Object v) = PriceLevel
    <$> (v .: "volume" >>= parseQuotedScientific)
    <*> (v .: "price"  >>= parseQuotedScientific)
  parseJSON v = typeMismatch "PriceLevel" v

--------------------------------------------------------------------------------

instance ToJSON Trade where
  toEncoding Trade {..} =
    pairs (
      "amount"    .= show amount <>
      "price"     .= show price <>
      "side"      .= side <>
      "timestamp" .= utcTimeToMS timestamp
    )

instance ToJSON PriceLevel where
  toEncoding PriceLevel {..} =
    pairs (
      "volume" .= show volume <>
      "price"  .= show price
    )
