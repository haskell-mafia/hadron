{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Request(
    parseHTTPRequest
  , addRequestHeader
  ) where

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import           Data.Semigroup ((<>))
import qualified Data.Text as T

import           Hadron.Data
import           Hadron.Error
import           Hadron.Parser.Request

import           P hiding ((<>))

-- | Take a ByteString representing one complete HTTP request and provide
-- either a parsed version or an error.
parseHTTPRequest :: ByteString -> Either RequestError HTTPRequest
parseHTTPRequest =
  first (RequestParsingError . T.pack) . AB.parseOnly httpRequestP

addRequestHeader :: HTTPRequest -> Header -> HTTPRequest
addRequestHeader (HTTPV1_1Request req) newH =
  let oldHs = unHTTPRequestHeaders $ hrqv1_1Headers req
      newHs = HTTPRequestHeaders $ (pure newH) <> oldHs in
  HTTPV1_1Request $ req { hrqv1_1Headers = newHs }

