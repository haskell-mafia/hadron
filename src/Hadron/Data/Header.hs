{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Data.Header(
    HeaderName(..)
  , HeaderValue(..)
  , Header(..)
  , renderHeaderName
  , renderHeaderValue
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty)

import           GHC.Generics (Generic)

import           P

-- | An HTTP header name is a case-insensitive arbitrary token.
newtype HeaderName =
  HeaderName {
    unHeaderName :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData HeaderName where rnf = genericRnf

renderHeaderName :: HeaderName -> ByteString
renderHeaderName = unHeaderName

-- | A single header value. Multiple of these can be sent using the same
-- 'HeaderName', in which case their canonical representation is as as a
-- single comma-delimited value.
--
-- RFC 7230 defines field values as consisting of any visible USASCII
-- characters.
newtype HeaderValue =
  HeaderValue {
    unHeaderValue :: ByteString
  } deriving (Eq, Show, Generic)

instance NFData HeaderValue where rnf = genericRnf

renderHeaderValue :: HeaderValue -> ByteString
renderHeaderValue = unHeaderValue

data Header =
  Header {
    httpHeaderName :: !HeaderName
  , httpHeaderValues :: !(NonEmpty HeaderValue)
  } deriving (Eq, Show, Generic)

instance NFData Header where rnf = genericRnf
