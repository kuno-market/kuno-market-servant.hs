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
import Data.Text (Text)

data SymbolInfo = SymbolInfo
  { name          :: Text
  , symbolType    :: Text
  , symbolDetails :: CurrencyPairDetails
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data CurrencyPairDetails = CurrencyPairDetails
  { baseCurrency  :: Text
  , quoteCurrency :: Text
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)

data CurrencyInfo = CurrencyInfo
  { name         :: Text
  , symbol       :: Text
  , abbreviation :: Text
  , precision    :: Int
} deriving (Show, Eq, Ord, Data, Generic, NFData, FromJSON, ToJSON)
