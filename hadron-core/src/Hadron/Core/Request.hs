{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Core.Request(
    parseHTTPRequest
  , addRequestHeader
  , lookupRequestHeader
  , removeRequestHeader
  , setRequestHeader
  ) where

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup ((<>))
import qualified Data.Text as T

import           Hadron.Core.Data
import           Hadron.Core.Error
import           Hadron.Core.Parser.Request

import           P hiding ((<>))

-- | Take a ByteString representing one complete HTTP request and provide
-- either a parsed version or an error.
parseHTTPRequest :: ByteString -> Either RequestError HTTPRequest
parseHTTPRequest =
  first (RequestParsingError . T.pack) . AB.parseOnly httpRequestP

-- | Return a request with a header added. This function does not do
-- any canonicalization; if the request already has a header with the
-- the provided name, the resulting request will have two Header
-- values with that HeaderName.
addRequestHeader :: HTTPRequest -> Header -> HTTPRequest
addRequestHeader (HTTPV1_1Request req) newH =
  let oldHs = unHTTPRequestHeaders $ hrqv1_1Headers req
      newHs = HTTPRequestHeaders $ (pure newH) <> oldHs in
  HTTPV1_1Request $ req { hrqv1_1Headers = newHs }

-- | Remove a header from a request by name. Returns Nothing' if this
-- would render the request invalid. Returns the request unchanged if
-- the header is not present.
removeRequestHeader :: HTTPRequest -> HeaderName -> Maybe' HTTPRequest
removeRequestHeader (HTTPV1_1Request req) hn
  | hn == HeaderName "host" = Nothing'
  | otherwise =
    let oldHs = unHTTPRequestHeaders $ hrqv1_1Headers req
        filteredHs = filter ((/= hn) . httpHeaderName) $ NE.toList oldHs in
    case nonEmpty filteredHs of
      Nothing ->
        Nothing'
      Just newHs ->
        pure . HTTPV1_1Request $ req { hrqv1_1Headers = HTTPRequestHeaders newHs }

-- | Set a request header by name, replacing any other existing values for
-- that header name.
--
-- For @setRequestHeader req name val@:
--
-- * If @req@ has no headers matching @name@, the header @Header name val@
--   is added (similar to 'addRequestHeader').
--
-- * If @req@ has one or more existing values associated with @name@, all of
--   these values are removed and replaced with @val@ (like
--   'removeRequestHeader' composed with 'addRequestHeader', but avoiding the
--   'Maybe''.
setRequestHeader :: HTTPRequest -> Header -> HTTPRequest
setRequestHeader (HTTPV1_1Request req) (Header hn hvs) =
  let
    oldHs = unHTTPRequestHeaders $ hrqv1_1Headers req
    filteredHs = filter ((/= hn) . httpHeaderName) $ NE.toList oldHs
    newHs = (Header hn hvs) :| filteredHs
  in
  HTTPV1_1Request $ req { hrqv1_1Headers = HTTPRequestHeaders newHs }

lookupRequestHeader :: HTTPRequest -> HeaderName -> Maybe' (NonEmpty HeaderValue)
lookupRequestHeader (HTTPV1_1Request req) hn =
  let matches = filter ((== hn) . httpHeaderName) . NE.toList . unHTTPRequestHeaders $ hrqv1_1Headers req in
  case nonEmpty matches of
    Nothing -> Nothing'
    Just matches' -> Just' . join $ httpHeaderValues <$> matches'
