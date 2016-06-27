{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Hadron.Data.Request(
    HTTPMethod(..)
  , HTTPRequest(..)
  , HTTPRequestHeaders(..)
  , HTTPRequestV1_1(..)
  , RequestBody(..)
  , renderHTTPMethod
  , renderHTTPRequest
  , renderHTTPRequestV1_1
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           GHC.Generics (Generic)

import           Hadron.Data.Header
import           Hadron.Data.Target
import           Hadron.Data.Version

import           P

data HTTPRequest =
    HTTPV1_1Request !HTTPRequestV1_1
  deriving (Eq, Show, Generic)

instance NFData HTTPRequest where rnf = genericRnf

renderHTTPRequest :: HTTPRequest -> ByteString
renderHTTPRequest (HTTPV1_1Request r) = renderHTTPRequestV1_1 r

data HTTPRequestV1_1 =
  HTTPRequestV1_1 {
    hrqv1_1Method :: !HTTPMethod
  , hrqv1_1Target :: !RequestTarget
  -- | Host header mandatory in 1.1.
  , hrqv1_1Headers :: !HTTPRequestHeaders
  , hrqv1_1Body :: !RequestBody
  } deriving (Eq, Show, Generic)

instance NFData HTTPRequestV1_1 where rnf = genericRnf

renderHTTPRequestV1_1 :: HTTPRequestV1_1 -> ByteString
renderHTTPRequestV1_1 (HTTPRequestV1_1 m rt (HTTPRequestHeaders hs) rb) = BS.concat [
    renderHTTPMethod m
  , " "
  , renderRequestTarget rt
  , " "
  , renderHTTPVersion HTTP_1_1
  , "\r\n"
  , BS.intercalate "\r\n" . NE.toList $ renderHeader <$> hs
  , "\r\n\r\n"
  , renderRequestBody rb
  ]

-- | We require at least a Host header.
newtype HTTPRequestHeaders =
  HTTPRequestHeaders {
    unHTTPRequestHeaders :: NonEmpty Header
  } deriving (Eq, Show, Generic)

instance NFData HTTPRequestHeaders where rnf = genericRnf

-- | HTTP method. This is an arbitrary token, and is case-sensitive.
newtype HTTPMethod =
  HTTPMethod {
    unHTTPMethod :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData HTTPMethod where rnf = genericRnf

renderHTTPMethod :: HTTPMethod -> ByteString
renderHTTPMethod = unHTTPMethod

data RequestBody =
    NoRequestBody
  -- | Regular non-chunked HTTP payload.
  | RequestBody !ByteString
  deriving (Eq, Show, Generic)

instance NFData RequestBody where rnf = genericRnf

renderRequestBody :: RequestBody -> ByteString
renderRequestBody NoRequestBody = ""
renderRequestBody (RequestBody bs) = bs
