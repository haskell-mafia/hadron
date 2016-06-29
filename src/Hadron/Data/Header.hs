{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Hadron.Data.Header(
    HeaderName(..)
  , HeaderValue(..)
  , Header(..)

  , hostHeader
  , renderHeader
  , renderHeaderName
  , renderHeaderValue
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.Bits ((.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           GHC.Generics (Generic)

import           P

-- | An HTTP header name is a case-insensitive arbitrary token.
newtype HeaderName =
  HeaderName {
    unHeaderName :: ByteString
  } deriving (Show, Generic)

instance NFData HeaderName where rnf = genericRnf

instance Eq HeaderName where
  (HeaderName x) == (HeaderName y) = (asciiToLower x) == (asciiToLower y)

-- | This should probably be in x-bytestring or somewhere.
asciiToLower :: ByteString -> ByteString
asciiToLower = {-# SCC asciiToLower #-} BS.map lower
  where
    lower w
      | w >= 0x41 && w <= 0x5a = w .|. 0x20
      | otherwise              = w
{-# INLINE asciiToLower #-}

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
  } deriving (Show, Generic)

instance NFData Header where rnf = genericRnf

instance Eq Header where
  (Header hn1 hvs1) == (Header hn2 hvs2) =
    hn1 == hn2 && canonicalize hvs1 == canonicalize hvs2
    where
      canonicalize = BS.intercalate "," . NE.toList . fmap renderHeaderValue

renderHeader :: Header -> ByteString
renderHeader (Header n vs) = BS.concat [
    renderHeaderName n
  , ":"
  , BS.intercalate "," (NE.toList $ renderHeaderValue <$> vs)
  ]

hostHeader :: HeaderName
hostHeader = HeaderName "host"
