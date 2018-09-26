# kuno-market-servant

Servant API specifications for Kuno Market's APIs. Use the `servant-api` package to generate functions for interacting with Kuno Market's APIs.

## Versioning

To use the v1 API, you need to checkout the `v1` branch. The `master` branch should be considered unstable.

## Network.KunoMarket.API.Public

This is the API spec for the public market-data API.

Note that the market-data API is serverd under the path `/public/{currency-pair}/{version}`. In particular, if you wish to communicate with the `v1` version of the market-data API for `BTC-ZAR` (i.e. `/public/BTC-ZAR/v1`), then you will need to use `servant-server`'s `BaseUrl` data type to set the base path.

For example:
```haskell literate
import Servant
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Network.KunoMarket.API.Types
import Network.KunoMarket.API.Public

getPublicOrderBook :: ClientM OrderBook
getPublicTicker    :: ClientM Ticker
getPublicTrades    :: ClientM [Trade]
getPublicOrderBook :<|> getPublicTicker :<|> getPublicTrades = client publicApi
```

<!--
```haskell literate
main :: IO ()
main = pure ()
```
-->
