{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Parser.Request(
    httpMethodP
  , httpRequestP
  , httpRequestV1_1P
  ) where

import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.List.NonEmpty as NE

import           Hadron.Data.Header
import           Hadron.Data.Request
import           Hadron.Data.Version
import           Hadron.Parser.Common
import           Hadron.Parser.Header
import           Hadron.Parser.Target

import           P

httpRequestP :: Parser HTTPRequest
httpRequestP = httpRequestV1_1P

httpRequestV1_1P :: Parser HTTPRequest
httpRequestV1_1P = do
  -- request line
  m <- httpMethodP
  void space
  rt <- requestTargetP
  void space
  void . ABC.string $ renderHTTPVersion HTTP_1_1
  skipCRLF
  hs <- httpRequestHeadersP
  skipCRLF
  skipCRLF
  payload <- do
    e <- AB.atEnd
    if e
      then pure NoRequestBody
      else fmap RequestBody AB.takeByteString
  pure . HTTPV1_1Request $ HTTPRequestV1_1 m rt hs payload
  where
    space = AB.word8 0x20

httpRequestHeadersP :: Parser HTTPRequestHeaders
httpRequestHeadersP = do
  hs <- AB.sepBy1' headerP skipCRLF
  case filter (\(Header n _) -> n == hostHeader) hs of
    [] -> fail "no host header"
    -- We don't fail on multiple host headers here; it looks invalid but
    -- this case isn't explicitly dealt with in the spec afaict.
    _ -> pure . HTTPRequestHeaders $ NE.fromList hs

httpMethodP :: Parser HTTPMethod
httpMethodP = HTTPMethod <$> AB.takeWhile isTokenWord
