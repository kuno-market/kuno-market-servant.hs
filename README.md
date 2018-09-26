# kuno-market-servant

Servant API specifications for Kuno Market's APIs. Use the `servant-api` package to generate functions for interacting with Kuno Market's APIs.

## Versioning

To use the v1 API, you need to checkout the `v1` branch. The `master` branch should be considered unstable.

## Network.KunoMarket.API.Public

This is the API spec for the public market-data API.

Note that the market-data API is serverd under the path `/public/{symbol}/{version}`. In particular, if you wish to communicate with the `v1` version of the market-data API for `BTC-ZAR` (i.e. `/public/BTC-ZAR/v1`), then you will need to use `servant-server`'s `BaseUrl` data type to set the base path.

<!--
```haskell literate
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
```
-->

You would generate the client functions like so:
```haskell literate
import Servant
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Network.KunoMarket.API.Public.Types
import Network.KunoMarket.API.Public
import Network.KunoMarket.API.Global.Types
import Network.KunoMarket.API.Global

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

Then you might use these functions so:
```haskell
main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  let env = ClientEnv manager' (BaseUrl Https "api.kuno.market" 443 "/public/BTC-ZAR/v1")
  res <- runClientM getPublicOrderBook env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _  -> putStrLn "Success!"
```

## Network.KunoMarket.API.Global

The "global" API returns public information about the exchange, which is not tied to a particular market/symbol.

You would generate the client functions thusly:
```haskell literate

getGlobalSymbols    :: ClientM [SymbolInfo]
getGlobalSymbol     :: String -> ClientM SymbolInfo
getGlobalCurrencies :: ClientM [CurrencyInfo]
getGlobalCurrency   :: String -> ClientM CurrencyInfo
getGlobalTimeStamp  :: ClientM Int
getGlobalSymbols :<|> getGlobalSymbol :<|> getGlobalCurrencies :<|> getGlobalCurrency :<|> getGlobalTimeStamp = client globalApi
```

Note that for compatibility with non-Haskell API clients, the timestamp is returned as an `Int` (miliseconds since the POSIX epoch) instead of `UTCTime`. You may use the following function to convert milisecond `Int`s to `UTCTime`:

```haskell
msToUTCTime :: Int -> UTCTime
msToUTCTime = posixSecondsToUTCTime . fromRational . (% 1e3) . toInteger
```

Since their is a single path for all global APIs (they do not have a symbol or version context), the `ClientEnv`'s `BaseUrl` should have an empty path:

```haskell
  ...
  let env = ClientEnv manager' (BaseUrl Https "api.kuno.market" 443 "")
  ...
```
