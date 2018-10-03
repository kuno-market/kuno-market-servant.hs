{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE PolyKinds         #-}

module Network.KunoMarket.API.Private where

import Servant
import Network.KunoMarket.API.Private.Types

type KunoBasicAuth = BasicAuth "Kuno Market private API" AuthedAccount

type OrderId = String
type WithdrawalId = String

type PrivateAPI =
       KunoBasicAuth :> "trades" :> Get '[JSON] [PrivateTrade]

  :<|> KunoBasicAuth :> "orders" :> Capture "order-id" String
                     :> DeleteNoContent '[JSON, PlainText] NoContent
  :<|> KunoBasicAuth :> "orders" :> Capture "order-id" String :> Get '[JSON] Order
  :<|> KunoBasicAuth :> "orders" :> QueryParam "status" [String] :> Get '[JSON] [Order]
  :<|> KunoBasicAuth :> "orders" :> ReqBody '[JSON] OrderRequest
                     :> PostAccepted '[JSON] String

  :<|> KunoBasicAuth :> "wallets" :> Get '[JSON] [Wallet]
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String :> Get '[JSON] [Wallet]
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String :> "fee" :> Get '[JSON] FeeStructure
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String :> "address"
                     :> Get '[JSON] AddressDetails
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String :> "trades"
                     :> Get '[JSON] [PrivateTrade]
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String :> "history"
                     :> QueryParam "from" Int :> QueryParam "to" Int
                     :> Get '[JSON] [Wallet]
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String :> "history" :> "pending"
                     :> Get '[JSON] [WalletMovement]
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String
                     :> ReqBody '[JSON] WithdrawalRequest :> "withdrawals"
                     :> PostAccepted '[JSON] String
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String
                     :> "withdrawals" :> Capture "withdrawal-id" WithdrawalId
                     :> Get '[JSON] WithdrawalRequestInformation
  :<|> KunoBasicAuth :> "wallets" :> Capture "symbol" String
                     :> "withdrawals" :> Capture "withdrawal-id" WithdrawalId
                     :> DeleteNoContent '[JSON] NoContent

privateApi :: Proxy PrivateAPI
privateApi = Proxy
