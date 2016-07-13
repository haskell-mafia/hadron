{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Core.Parser.Request(
    httpMethodP
  , httpRequestP
  , httpRequestV1_1P
  ) where

import           Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import           Data.List.NonEmpty (nonEmpty)

import           Hadron.Core.Data.Request
import           Hadron.Core.Data.Version
import           Hadron.Core.Parser.Common
import           Hadron.Core.Parser.Header
import           Hadron.Core.Parser.Target

import           P

httpRequestP :: Parser HTTPRequest
httpRequestP = httpRequestV1_1P

httpRequestV1_1P :: Parser HTTPRequest
httpRequestV1_1P = do
  -- request line
  m <- httpMethodP <?> "httpMethodP"
  void space
  rt <- requestTargetP <?> "requestTargetP"
  void space
  _ <- ABC.string (renderHTTPVersion HTTP_1_1) <?> "HTTP version"
  skipCRLF
  hs <- httpRequestHeadersP <?> "httpRequestHeadersP"
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
  case nonEmpty hs of
    Nothing -> fail "empty header list"
    Just hs' -> maybe' (fail "no host header") pure $ requestHeaders hs'

httpMethodP :: Parser HTTPMethod
httpMethodP = HTTPMethod <$> AB.takeWhile isTokenWord
