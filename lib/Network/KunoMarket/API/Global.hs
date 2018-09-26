{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Network.KunoMarket.API.Global
  ( GlobalAPI
  , globalApi
  ) where

import Servant
import Network.KunoMarket.API.Global.Types

-- $setup
-- >>> import Servant.Server
-- >>> import Data.Text

-- | The global API layout is as follows:
--
-- >>> putStrLn $ unpack $ layout globalApi
-- /
-- └─ public/
--    ├─ currencies/
--    │  ├─•
--    │  ┆
--    │  ┆
--    │  └─ <capture>/
--    │     └─•
--    ├─ symbols/
--    │  ├─•
--    │  ┆
--    │  ┆
--    │  └─ <capture>/
--    │     └─•
--    └─ timestamp/
--       └─•
-- <BLANKLINE>
--

type GlobalAPI =
       "public" :> "symbols"    :> Get '[JSON] [SymbolInfo]
  :<|> "public" :> "symbols"    :> Capture "symbol" String :> Get '[JSON] SymbolInfo
  :<|> "public" :> "currencies" :> Get '[JSON] [CurrencyInfo]
  :<|> "public" :> "currencies" :> Capture "currency" String :> Get '[JSON] CurrencyInfo
  :<|> "public" :> "timestamp"  :> Get '[JSON] Int

globalApi :: Proxy GlobalAPI
globalApi = Proxy
