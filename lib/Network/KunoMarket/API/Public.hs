{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Network.KunoMarket.API.Public
  ( PublicAPI
  , publicApi
  ) where

import Servant
import Network.KunoMarket.API.Public.Types

-- $setup
-- >>> import Servant.Server
-- >>> import Data.Text

-- | Ignoring the symbol and version as specified in the path (e.g. __\/public\/BTC-ZAR\/v1__),
-- the public API layout is as follows:
--
-- >>> putStrLn $ unpack $ layout publicApi
-- /
-- ├─ order-book/
-- │  └─•
-- ├─ ticker/
-- │  └─•
-- └─ trades/
--    └─•
-- <BLANKLINE>

type PublicAPI =
       "order-book" :> Get '[JSON] OrderBook
  :<|> "ticker"     :> Get '[JSON] Ticker
  :<|> "trades"     :> Get '[JSON] [Trade]

publicApi :: Proxy PublicAPI
publicApi = Proxy
