{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Wai.Error(
    WaiRequestError(..)
  , renderWaiRequestError
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

data WaiRequestError =
    WaiNoHostHeader
  | WaiNoRequestHeaders
  | WaiInvalidRequestHeaders
  | WaiInvalidRequestMethod
  | WaiInvalidRequestTarget
  deriving (Eq, Show, Generic)

instance NFData WaiRequestError where rnf = genericRnf

renderWaiRequestError :: WaiRequestError -> Text
renderWaiRequestError WaiNoHostHeader = "no host header"
renderWaiRequestError WaiNoRequestHeaders = "no request headers - at least one is required"
renderWaiRequestError WaiInvalidRequestHeaders = "invalid request headers"
renderWaiRequestError WaiInvalidRequestMethod = "invalid request method"
renderWaiRequestError WaiInvalidRequestTarget = "invalid request target"
