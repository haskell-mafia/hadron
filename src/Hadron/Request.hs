{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Request(
    parseHTTPRequest
  ) where

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import qualified Data.Text as T

import           Hadron.Data.Request
import           Hadron.Error
import           Hadron.Parser.Request

import           P

-- | Take a ByteString representing one complete HTTP request and provide
-- either a parsed version or an error.
parseHTTPRequest :: ByteString -> Either RequestError HTTPRequest
parseHTTPRequest =
  first (RequestParsingError . T.pack) . AB.parseOnly httpRequestP
