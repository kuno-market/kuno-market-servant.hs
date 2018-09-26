{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumDecimals #-}

module Network.KunoMarket.API.Common.Types
  ( OrderSide(..)
  , OrderPriceType(..)
  , PostOnlyType(..)
  , TimeInForce(..)
  , utcTimeToMS
  , msToUTCTime
  , parseQuotedScientific
  ) where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Data (Data)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), Value(..), ToJSON(..),
                   genericParseJSON, constructorTagModifier, defaultOptions, camelTo2)
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Text (unpack, words, toUpper, pack)
import Data.Ratio ((%))
import Text.Read (readMaybe)
import Data.Scientific
import Data.Monoid ((<>))

data OrderSide = Buy | Sell
  deriving (Show, Read, Eq, Ord, Data, Generic, NFData, ToJSON)

data OrderPriceType = Market | Limit Scientific
  deriving (Show, Read, Eq, Ord, Data, Generic, NFData)

data TimeInForce
  = GoodTillCancelled
  | GoodTillTime UTCTime
  | ImmediateOrCancel
  deriving (Generic, Show, Read, Eq, NFData, Ord)

data PostOnlyType
  = PostOnly
  | NoPostOnly
  deriving (Show, Read, Eq, Generic, NFData, Ord)

--------------------------------------------------------------------------------

instance FromJSON TimeInForce where
  parseJSON (String s) =
    case Data.Text.words $ toUpper s of
      ["GOOD_TILL_CANCELLED"] -> pure GoodTillCancelled
      ["IMMEDIATE_OR_CANCEL"] -> pure ImmediateOrCancel
      ["GOOD_TILL_TIME", tm] -> do
        let mtm = msToUTCTime <$> readMaybe (unpack tm)
        case mtm of
          Nothing -> undefined
          Just t  -> pure $ GoodTillTime t
      _ -> fail $ "Unknown time-in-force type: " <> show s
  parseJSON s = typeMismatch "TimeInForce" s

instance FromJSON OrderPriceType where
  parseJSON (String p) =
    case Data.Text.words $ toUpper p of
      ["MARKET"]   -> pure Market
      ["LIMIT", q] ->
        case readMaybe $ unpack q of
          Nothing -> fail $ "Couldn't parse limit price: " <> show p
          Just pr -> pure $ Limit pr
      _ -> fail $ "Unknown order price type: " <> show p
  parseJSON s = typeMismatch "OrderPrice" s

instance FromJSON OrderSide where
  parseJSON (String s) =
    case toUpper s of
      "BUY"  -> pure Buy
      "SELL" -> pure Sell
      _ -> fail $ "Unknown order-side type: " <> show s
  parseJSON s = typeMismatch "OrderSide" s

instance FromJSON PostOnlyType where
  parseJSON = genericParseJSON
    $ defaultOptions {constructorTagModifier = camelTo2 '_'}

--------------------------------------------------------------------------------

instance ToJSON TimeInForce where
  toJSON GoodTillCancelled = String "good_till_cancelled"
  toJSON ImmediateOrCancel = String "immediate_or_cancel"
  toJSON (GoodTillTime t)  =
    String $ "good_till_time " <> pack (show $ utcTimeToMS t)

instance ToJSON OrderPriceType where
  toJSON Market = String "market"
  toJSON (Limit p) = String ("limit " <> pack (show p))

instance ToJSON PostOnlyType where
  toJSON NoPostOnly = String "no_post_only"
  toJSON PostOnly = String "post_only"

--------------------------------------------------------------------------------

utcTimeToMS :: UTCTime -> Int
utcTimeToMS = round @Double . (* 1e3) . realToFrac . utcTimeToPOSIXSeconds

msToUTCTime :: Int -> UTCTime
msToUTCTime = posixSecondsToUTCTime . fromRational . (% 1e3) . toInteger

parseQuotedScientific :: Value -> Parser Scientific
parseQuotedScientific (String q) =
  case readMaybe $ unpack q of
    Nothing -> fail "failure parsing quoted decimal"
    Just d  -> pure d
parseQuotedScientific (Number q) = pure q
parseQuotedScientific v = typeMismatch "Number" v
