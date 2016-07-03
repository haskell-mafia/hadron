{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Request(
    parseHTTPRequest
  , addRequestHeader
  , lookupRequestHeader
  ) where

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
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

lookupRequestHeader :: HTTPRequest -> HeaderName -> Maybe' (NonEmpty HeaderValue)
lookupRequestHeader (HTTPV1_1Request req) hn =
  let matches = filter ((== hn) . httpHeaderName) . NE.toList . unHTTPRequestHeaders $ hrqv1_1Headers req in
  case nonEmpty matches of
    Nothing -> Nothing'
    Just matches' -> Just' . join $ httpHeaderValues <$> matches'
