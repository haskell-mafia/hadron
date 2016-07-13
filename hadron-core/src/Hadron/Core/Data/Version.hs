{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Core.Data.Version(
    HTTPVersion(..)
  , parseHTTPVersion
  , renderHTTPVersion
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

-- | HTTP protocol version. We do not include HTTP/2 as its semantics
-- don't concern us (belong in higher-level libraries), and we do not
-- include HTTP/1.0 as we don't support it at the moment (can change
-- if there's ever a need).
data HTTPVersion =
    HTTP_1_1
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData HTTPVersion where rnf = genericRnf

renderHTTPVersion :: HTTPVersion -> ByteString
renderHTTPVersion HTTP_1_1 = "HTTP/1.1"

parseHTTPVersion :: ByteString -> Maybe' HTTPVersion
parseHTTPVersion "HTTP/1.1" = pure HTTP_1_1
parseHTTPVersion _ = Nothing'
