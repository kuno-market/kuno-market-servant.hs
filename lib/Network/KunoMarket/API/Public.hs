{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Network.KunoMarket.API.Public where

import Servant
import Data.ByteString
import Network.KunoMarket.API.RawJSON

type PublicAPI =
       "order-book" :> Get '[RawJSON] ByteString
  :<|> "ticker"     :> Get '[RawJSON] ByteString
  :<|> "trades"     :> Get '[RawJSON] ByteString

publicApi :: Proxy PublicAPI
publicApi = Proxy
