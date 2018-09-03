{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Network.KunoMarket.API.Public where

import Servant
import Network.KunoMarket.API.Public.Types

type PublicAPI =
       "order-book" :> Get '[JSON] OrderBook
  :<|> "ticker"     :> Get '[JSON] Ticker
  :<|> "trades"     :> Get '[JSON] Trades

publicApi :: Proxy PublicAPI
publicApi = Proxy
