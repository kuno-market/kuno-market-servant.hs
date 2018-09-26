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
  { name          :: String
  , symbolType    :: String
  , symbolDetails :: CurrencyPairDetails
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data CurrencyPairDetails = CurrencyPairDetails
  { baseCurrency  :: String
  , quoteCurrency :: String
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data CurrencyInfo = CurrencyInfo
  { name         :: String
  , symbol       :: String
  , abbreviation :: String
  , precision    :: Int
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)
