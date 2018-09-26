{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Network.KunoMarket.API.Global.Types
  ( SymbolInfo(..)
  , CurrencyInfo(..)
  , CurrencyPairDetails(..)
  ) where

import Data.Data (Data)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON(..))

data SymbolInfo = SymbolInfo
  { name          :: String -- ^ e.g. \"BTC-ZAR\"
  , symbolType    :: String -- ^ For now, only \"currency_pair\"
  , symbolDetails :: CurrencyPairDetails -- ^ For now, we only have currency pairs as symbols.
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data CurrencyPairDetails = CurrencyPairDetails
  { baseCurrency  :: String -- ^ e.g. \"BTC\"
  , quoteCurrency :: String -- ^ e.g. \"ZAR\"
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data CurrencyInfo = CurrencyInfo
  { name         :: String -- ^ e.g. \"Bitcoin\"
  , symbol       :: String -- ^ e.g. \"Ƀ\"
  , abbreviation :: String -- ^ e.g. \"BTC\"
  , precision    :: Int    -- ^ e.g. 1e8
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)
