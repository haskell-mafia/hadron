{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Wai.Error(
    WaiRequestError(..)
  , renderWaiRequestError
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.Text as T

import           GHC.Generics (Generic)

import           P

data WaiRequestError =
    WaiNoHostHeader
  | WaiNoRequestHeaders
  | WaiInvalidRequestHeaders
  | WaiInvalidRequestMethod !ByteString
  | WaiInvalidRequestTarget !ByteString
  | WaiUnsupportedHTTPVersion !Int !Int
  deriving (Eq, Show, Generic)

instance NFData WaiRequestError where rnf = genericRnf

renderWaiRequestError :: WaiRequestError -> Text
renderWaiRequestError WaiNoHostHeader = "no host header"
renderWaiRequestError WaiNoRequestHeaders = "no request headers - at least one is required"
renderWaiRequestError WaiInvalidRequestHeaders = "invalid request headers"
renderWaiRequestError (WaiInvalidRequestMethod bs) = T.unwords [
    "invalid request method:"
  , T.pack $ show bs
  ]
renderWaiRequestError (WaiInvalidRequestTarget bs) = T.unwords [
    "invalid request target:"
  , T.pack $ show bs
  ]
renderWaiRequestError (WaiUnsupportedHTTPVersion major minor) = T.concat [
    "unsupported HTTP version: HTTP"
  , renderIntegral major
  , "/"
  , renderIntegral minor
  ]
