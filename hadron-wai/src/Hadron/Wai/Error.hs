{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Wai.Error(
    WaiRequestError(..)
  , renderWaiRequestError
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import qualified Data.Text as T

import           GHC.Generics (Generic)

import           P

data WaiRequestError =
    WaiNoHostHeader
  | WaiNoRequestHeaders
  | WaiInvalidRequestHeaders
  | WaiInvalidRequestMethod
  | WaiInvalidRequestTarget
  | WaiUnsupportedHTTPVersion !Int !Int
  deriving (Eq, Show, Generic)

instance NFData WaiRequestError where rnf = genericRnf

renderWaiRequestError :: WaiRequestError -> Text
renderWaiRequestError WaiNoHostHeader = "no host header"
renderWaiRequestError WaiNoRequestHeaders = "no request headers - at least one is required"
renderWaiRequestError WaiInvalidRequestHeaders = "invalid request headers"
renderWaiRequestError WaiInvalidRequestMethod = "invalid request method"
renderWaiRequestError WaiInvalidRequestTarget = "invalid request target"
renderWaiRequestError (WaiUnsupportedHTTPVersion major minor) = T.concat [
    "unsupported HTTP version: HTTP"
  , renderIntegral major
  , "/"
  , renderIntegral minor
  ]
