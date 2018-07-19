{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.KunoMarket.API.RawJSON where

import Servant
import qualified Data.ByteString as BS
import qualified Network.HTTP.Media as M
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as LBS

data RawJSON

instance Accept RawJSON where
    contentTypes _ =
      "application" M.// "json" M./: ("charset", "utf-8") NE.:|
      [ "application" M.// "json" ]

instance MimeRender RawJSON BS.ByteString where
  mimeRender _ = LBS.fromStrict

instance MimeRender RawJSON LBS.ByteString where
  mimeRender _ = id

instance MimeUnrender RawJSON LBS.ByteString where
  mimeUnrender _ = Right

instance MimeUnrender RawJSON BS.ByteString where
  mimeUnrender _ = Right . LBS.toStrict
