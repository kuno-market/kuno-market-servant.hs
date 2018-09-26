{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE PolyKinds         #-}

module Network.KunoMarket.API.Global where

import Servant
import Network.KunoMarket.API.Global.Types
import Data.Text

type GlobalAPI =
       "public" :> "symbols"    :> Get '[JSON] [SymbolInfo]
  :<|> "public" :> "symbols"    :> Capture "symbol" Text :> Get '[JSON] SymbolInfo
  :<|> "public" :> "currencies" :> Get '[JSON] [CurrencyInfo]
  :<|> "public" :> "currencies" :> Capture "currency" Text :> Get '[JSON] CurrencyInfo
  :<|> "public" :> "timestamp"  :> Get '[PlainText] Text

globalApi :: Proxy GlobalAPI
globalApi = Proxy
