import Servant
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Data.ByteString hiding (putStrLn)
import Network.KunoMarket.API.Public

--This file mrely needs to compile for the test to be considered successful.

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
