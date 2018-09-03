{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE PolyKinds         #-}

module Network.KunoMarket.API.Global where

import Servant
import Network.KunoMarket.API.Global.Types
import Data.Text

type GlobalAPI =
       "public" :> "symbols"
                :> Get '[JSON] (Headers '[Header "Cache-Control" Text] [SymbolInfo])
  :<|> "public" :> "symbols" :> Capture "symbol" Text
                :> Get '[JSON] (Headers '[Header "Cache-Control" Text] SymbolInfo)
  :<|> "public" :> "currencies"
                :> Get '[JSON] (Headers '[Header "Cache-Control" Text] [CurrencyInfo])
  :<|> "public" :> "currencies" :> Capture "currency" Text
                :> Get '[JSON] (Headers '[Header "Cache-Control" Text] CurrencyInfo)
  :<|> "public" :> "timestamp"  :> Get '[PlainText] Text

globalApi :: Proxy GlobalAPI
globalApi = Proxy
