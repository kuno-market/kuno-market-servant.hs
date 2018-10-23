{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Network.KunoMarket.API.Private.Types
  ( AuthedAccount(..)
  , OrderSide(..)
  , OrderRequest(..)
  , Wallet(..)
  , Balance(..)
  , BalanceType(..)
  , PrivateTrade(..)
  , AddressDetails(..)
  , WithdrawalRequest(..)
  , WithdrawalRequestInformation(..)
  , FeeStructure(..)
  , WalletMovement(..)
  , TakeProfitType(..)
  , StopLossType(..)
  , OrderFeature(..)
  , Order(..)
  , OrderEvent(..)
  ) where

import Data.Time
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Dynamic (Dynamic)
import Control.DeepSeq (NFData)
import Data.Scientific
import Data.Monoid ((<>))
import Network.KunoMarket.API.Common.Types (OrderSide(..), OrderPriceType(..),
                                            PostOnlyType(..), TimeInForce(..),
                                            utcTimeToMS, msToUTCTime,
                                            parseQuotedScientific)
import Data.Aeson (FromJSON(..), Value(..), (.:), (.:?), (.=), ToJSON(..), pairs,
                   genericParseJSON, genericToEncoding, withObject, object,
                   constructorTagModifier, defaultOptions, camelTo2)
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import Control.Monad (unless)
import Data.Maybe (isJust, fromJust)
import Data.List ((\\))
import Data.Text (unpack, pack, toUpper, words)
import Text.Read (readMaybe)

newtype AuthedAccount = AuthedAccount {unAuthedAccount :: Dynamic}

data OrderRequest = OrderRequest
  { size      :: Scientific
    -- ^ For a Limit order, this should be in Base currency units (e.g. BTC).
    --   For a Market Buy order, this should be in Quote currency units (e.g. ZAR you want to buy with).
    --   For a Market Sell order, this should be in Base currency units (e.g. BTC you want to sell).
  , side      :: OrderSide
  , priceType :: OrderPriceType
  , symbol    :: String -- ^ For now, a currency pair such as \"BTC-ZAR\".
  , features  :: [OrderFeature]
    -- ^ A set of modifiers to add to this request. Note that a specific 'OrderFeature' \"type\"
    --   should only appear once (for instance, one can't specify two 'FeatureComments'\' at
    --   once), and some combinations are forbidden (for instance, 'FeaturePostOnly PostOnly'
    --   is incompatible with 'Market' orders). All such illegal combinations will be rejected.
} deriving (Show, Read, Eq, Generic, NFData)

data StopLossType
  = NoStopLoss
  | VanillaStopLoss Scientific -- ^ Absolute price.
  | TrailingStopLossNominal Scientific -- ^ Absolute difference from price.
  | TrailingStopLossPercentage Scientific -- ^ Percentage difference from price.
  deriving (Show, Read, Eq, Generic, NFData, Ord)

data TakeProfitType
  = NoTakeProfit
  | VanillaTakeProfit Scientific -- ^ Absolute price.
  | TrailingTakeProfitNominal Scientific -- ^ Absolute difference from price.
  | TrailingTakeProfitPercentage Scientific -- ^ Percentage difference from price.
  deriving (Show, Read, Eq, Generic, NFData, Ord)

data OrderFeature
  = FeatureComments String -- ^ A simple, arbitrary comment to attach to the order.
  | FeatureStopLoss StopLossType
  | FeatureTakeProfit TakeProfitType
  | FeaturePostOnly PostOnlyType -- ^ 'PostOnly' is incompatible with 'Market' orders.
  | FeatureTimeInForce TimeInForce
  deriving (Show, Read, Eq, Generic, NFData, Ord)

data Wallet = Wallet
  { asset    :: String -- ^ For now, a currency (e.g. \"BTC\").
  , walletId :: String
  , balances :: [Balance]
} deriving (Show, Read, Eq, Generic, NFData, ToJSON, FromJSON)

--FIXME: document this...
data WalletMovement = WalletMovement
  { asset     :: String
  , amount    :: Scientific -- ^ This will be in the asset's units.
  , timestamp :: UTCTime
  , reason    :: WalletMovementReason
    -- ^ Possible values include \"withdrawal\", \"deposit\", \"trade\", \"reservation\" ()
} deriving (Show, Read, Eq, Generic, NFData)

type WalletMovementReason = String

data Balance = Balance
  { balanceType :: BalanceType
  , amount      :: Scientific
} deriving (Show, Read, Eq, Generic, NFData)

data BalanceType
  = Nominal -- ^ Funds unused
  | NominalReserved -- ^ Funds reserved for limit order
  | Withdrawing -- ^ Funds marked to be withdrawn
  deriving (Show, Read, Eq, Generic, NFData)

data PrivateTrade = PrivateTrade
  { volume    :: Scientific --FIXME: should this be amount, instead?
  , price     :: Scientific
  , side      :: OrderSide
  , timestamp :: UTCTime
  , asset     :: String
  , orderId   :: String
} deriving (Show, Eq, Ord, Data, Generic, NFData)

data AddressDetails = AddressDetails
  { asset       :: String
  , destination :: String
} deriving (Show, Read, Eq, Generic, NFData, ToJSON, FromJSON)

data WithdrawalRequest = WithdrawalRequest
  { amount      :: Scientific
  , destination :: Maybe String
} deriving (Show, Read, Eq, Generic, NFData)

data WithdrawalRequestInformation = WithdrawalRequestInformation
  { amount      :: Scientific
  , asset       :: String
  , destination :: Maybe String
  , timestamp   :: UTCTime
  , status      :: String
} deriving (Show, Read, Eq, Generic, NFData)

data FeeStructure = FeeStructure
  { makerFee :: Scientific
  , takerFee :: Scientific
} deriving (Show, Read, Eq, Generic, NFData)

data Order = Order
  { size          :: Scientific
  , remainingSize :: Scientific
  , side          :: OrderSide
  , priceType     :: OrderPriceType
  , timeInForce   :: TimeInForce
  , symbol        :: String
  , stopLoss      :: StopLossType
  , takeProfit    :: TakeProfitType
  , comments      :: Maybe String
  , events        :: [OrderEvent]
  , timestamp     :: UTCTime
  , orderId       :: String
}

data OrderEvent = OrderEvent
  { timestamp :: UTCTime
  , event     :: String
}

--------------------------------------------------------------------------------

instance FromJSON Balance where
  parseJSON = genericParseJSON
    $ defaultOptions {constructorTagModifier = camelTo2 '_'}

instance FromJSON BalanceType where
  parseJSON = genericParseJSON
    $ defaultOptions {constructorTagModifier = camelTo2 '_'}

instance FromJSON WithdrawalRequest where
  parseJSON (Object v) = WithdrawalRequest
    <$> (v .: "amount" >>= parseQuotedScientific)
    <*>  v .: "destination"
  parseJSON v = typeMismatch "WithdrawalRequest" v

instance FromJSON WithdrawalRequestInformation where
  parseJSON (Object v) = WithdrawalRequestInformation
    <$> (v .: "amount" >>= parseQuotedScientific)
    <*>  v .: "asset"
    <*>  v .: "destination"
    <*>  fmap msToUTCTime (v .: "timestamp")
    <*>  v .: "status"
  parseJSON v = typeMismatch "WithdrawalRequestInformation" v

instance FromJSON PrivateTrade where
  parseJSON (Object v) = PrivateTrade
    <$> (v .: "volume" >>= parseQuotedScientific)
    <*> (v .: "price" >>= parseQuotedScientific)
    <*>  v .: "side"
    <*>  fmap msToUTCTime (v .: "timestamp")
    <*>  v .: "asset"
    <*>  v .: "order_id"
  parseJSON v = typeMismatch "PrivateTrade" v

instance FromJSON WalletMovement where
  parseJSON (Object v) = WalletMovement
    <$>  v .: "asset"
    <*> (v .: "amount" >>= parseQuotedScientific)
    <*>  fmap msToUTCTime (v .: "timestamp")
    <*>  v .: "reason"
  parseJSON v = typeMismatch "WalletMovement" v

instance FromJSON FeeStructure where
  parseJSON (Object v) = FeeStructure
    <$> (v .: "maker_fee" >>= parseQuotedScientific)
    <*> (v .: "taker_fee" >>= parseQuotedScientific)
  parseJSON v = typeMismatch "FeeStructure" v

instance FromJSON StopLossType where
  parseJSON (String s) =
    case Data.Text.words $ toUpper s of
      ["NO_STOP_LOSS"] -> pure NoStopLoss
      ["VANILLA_STOP_LOSS", q] ->
        case readMaybe $ unpack q of
          Nothing -> fail $ "Couldn't parse vanilla stop loss price: " <> show s
          Just pr -> pure $ VanillaStopLoss pr
      ["TRAILING_STOP_LOSS_NOMINAL", d] ->
        case readMaybe $ unpack d of
          Nothing -> fail $ "Couldn't parse trailing stop loss price: " <> show s
          Just pr -> pure $ TrailingStopLossNominal pr
      ["TRAILING_STOP_LOSS_PERCENTAGE", p] ->
        case readMaybe $ unpack p of
          Nothing -> fail $ "Couldn't parse trailing stop loss percentage: " <> show s
          Just pr -> pure $ TrailingStopLossPercentage pr
      _ -> fail $ "Unknown stop loss type: " <> show s
  parseJSON s = typeMismatch "StopLossType" s

instance FromJSON TakeProfitType where
  parseJSON (String s) =
    case Data.Text.words $ toUpper s of
      ["NO_TAKE_PROFIT"] -> pure NoTakeProfit
      ["VANILLA_TAKE_PROFIT", q] ->
        case readMaybe $ unpack q of
          Nothing -> fail $ "Couldn't parse vanilla take profit price: " <> show s
          Just pr -> pure $ VanillaTakeProfit pr
      ["TRAILING_TAKE_PROFIT_NOMINAL", d] ->
        case readMaybe $ unpack d of
          Nothing -> fail $ "Couldn't parse trailing take profit price: " <> show s
          Just pr -> pure $ TrailingTakeProfitNominal pr
      ["TRAILING_TAKE_PROFIT_PERCENTAGE", p] ->
        case readMaybe $ unpack p of
          Nothing -> fail $ "Couldn't parse trailing take profit percentage: " <> show s
          Just pr -> pure $ TrailingTakeProfitPercentage pr
      _ -> fail $ "Unknown take profit type: " <> show s
  parseJSON s = typeMismatch "TakeProfitType" s

knownFields :: [String]
knownFields = ["size", "side", "price_type", "symbol", "features"]

instance FromJSON OrderRequest where
  parseJSON = withObject "create-order-request-details" $ \o -> do
    let unknownFields = (\\ knownFields) $ (unpack . fst) <$> HM.toList o
    unless (null unknownFields)
      $ fail $ "unknown field(s): " <> show unknownFields
    size <- o .: "size" >>= parseQuotedScientific
    tside <- o .: "side"
    tpricetype <- o .: "price_type"
    tsymbol <- o .: "symbol"
    mfeatures <- o .:? "features"

    side <- parseJSON tside

    pricetype <- parseJSON tpricetype

    symbol <- parseJSON tsymbol

    features <-
      case mfeatures of
        Nothing -> pure []
        Just features ->
          ($ features) $ withObject "feature-set" $ \f -> do
            let unknownFeatures = (\\ knownFeatures) $ (unpack . fst) <$> HM.toList f
            unless (null unknownFeatures)
              $ fail $ "unknown feature(s): " <> show unknownFeatures

            ftif <- f .:? "time_in_force"
            comments <- f .:? "comments"
            fpostonly <- f .:? "post_only"
            ftakeprofit <- f .:? "take_profit"
            fstoploss <- f .:? "stop_loss"

            postonly <- case fpostonly of
              Nothing -> pure Nothing
              Just po -> Just <$> parseJSON po

            takeprofit <- case ftakeprofit of
              Nothing -> pure Nothing
              Just tf -> Just <$> parseJSON tf

            stoploss <- case fstoploss of
              Nothing -> pure Nothing
              Just sl -> Just <$> parseJSON sl

            tif <- case ftif of
              Nothing -> pure Nothing
              Just tf -> Just <$> parseJSON tf

            --bad combinations of features will be rejected later by the exchange
            pure $!
              [FeatureComments $ fromJust comments | isJust comments]
              <> [FeatureStopLoss $ fromJust stoploss | isJust stoploss]
              <> [FeatureTakeProfit $ fromJust takeprofit | isJust takeprofit]
              <> [FeaturePostOnly $ fromJust postonly | isJust postonly]
              <> [FeatureTimeInForce $ fromJust tif | isJust tif]
    pure (OrderRequest size side pricetype symbol features)

knownFeatures :: [String]
knownFeatures = ["comments", "post_only", "take_profit", "stop_loss", "time_in_force"]

--------------------------------------------------------------------------------

instance ToJSON OrderRequest where
  toJSON OrderRequest {..} =
    object [
      "size"       .= show size,
      "side"       .= side,
      "price_type" .= priceType,
      "symbol"     .= symbol,
      "features"   .= object (fmap featurePair features)
    ]
    where
      featurePair (FeatureComments comm) = ("comments", toJSON comm)
      featurePair (FeatureStopLoss slt) = ("stop_loss", toJSON slt)
      featurePair (FeatureTakeProfit tpt) = ("take_profit", toJSON tpt)
      featurePair (FeaturePostOnly pot) = ("post_only", toJSON pot)
      featurePair (FeatureTimeInForce tif) = ("time_in_force", toJSON tif)

instance ToJSON Balance where
  toEncoding = genericToEncoding
    $ defaultOptions {constructorTagModifier = camelTo2 '_'}

instance ToJSON BalanceType where
  toEncoding = genericToEncoding
    $ defaultOptions {constructorTagModifier = camelTo2 '_'}

instance ToJSON WithdrawalRequest where
  toEncoding WithdrawalRequest {..} =
    pairs (
      "amount"      .= show amount <>
      "destination" .= destination
    )

instance ToJSON PrivateTrade where
  toEncoding PrivateTrade {..} =
    pairs (
       "volume"    .= show volume <>
       "price"     .= show price <>
       "side"      .= side <>
       "timestamp" .= utcTimeToMS timestamp <>
       "asset"     .= asset <>
       "order_id"  .= orderId
    )

instance ToJSON WalletMovement where
  toEncoding WalletMovement {..} =
    pairs (
      "asset"     .= asset <>
      "amount"    .= show amount <>
      "timestamp" .= utcTimeToMS timestamp <>
      "reason"    .= reason
    )

instance ToJSON FeeStructure where
  toEncoding FeeStructure {..} =
    pairs (
      "maker_fee" .= show makerFee <>
      "taker_fee" .= show takerFee
    )

instance ToJSON WithdrawalRequestInformation where
  toEncoding WithdrawalRequestInformation {..} =
    pairs (
      "amount"      .= show amount <>
      "asset"       .= asset <>
      "destination" .= destination <>
      "timestamp"   .= utcTimeToMS timestamp <>
      "status"      .= status
    )

instance ToJSON TakeProfitType where
  toJSON NoTakeProfit = String "no_take_profit"
  toJSON (VanillaTakeProfit am) = String $ " vanilla_take_profit " <> pack (show am)
  toJSON (TrailingTakeProfitNominal tr) = String $ "trailing_take_profit_nominal " <> pack (show tr)
  toJSON (TrailingTakeProfitPercentage pc) = String $ "trailing_take_profit_percentage " <> pack (show pc)

instance ToJSON StopLossType where
  toJSON NoStopLoss = String "no_stop_loss"
  toJSON (VanillaStopLoss am) = String $ "vanilla_stop_loss " <> pack (show am)
  toJSON (TrailingStopLossNominal tr) = String $ "trailing_stop_loss_nominal " <> pack (show tr)
  toJSON (TrailingStopLossPercentage pc) = String $ "trailing_stop_loss_percentage " <> pack (show pc)
