# kuno-market-servant

Servant API specifications for Kuno Market's APIs. Use the `servant-api` package to generate functions for interacting with Kuno Market's APIs.

APIs return pure, schemaless JSON as `ByteString`s. You may use `lens-aeson` to inspect the payloads.

## Network.KunoMarket.API.Public

This is the API spec for the public market-data API.

Note that the market-data API is serverd under the path `/public/{currency-pair}/{version}`. In particular, if you wish to communicate with the `v1` version of the market-data API for `BTC-ZAR` (i.e. `/public/BTC-ZAR/v1`), then you will need to use `servant-server`'s `BaseUrl` data type to set the base path.

For example:
```Haskell

import Servant
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Data.ByteString hiding (putStrLn)
import Network.KunoMarket.API.Public

getOrderBook :: ClientM ByteString
getTicker    :: ClientM ByteString
getTrades    :: ClientM ByteString
getOrderBook :<|> getTicker :<|> getTrades = client publicApi

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "api.kuno.market" 443 "public/BTC-ZAR/v1")
  res <- runClientM getOrderBook env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right book -> print book
```
