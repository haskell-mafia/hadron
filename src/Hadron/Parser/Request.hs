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

import           Hadron.Data.Request
import           Hadron.Data.Version
import           Hadron.Parser.Common
import           Hadron.Parser.Header
import           Hadron.Parser.Target

import           P

httpRequestP :: Parser HTTPRequest
httpRequestP = httpRequestV1_1P

-- FIXME: limit length
httpRequestV1_1P :: Parser HTTPRequest
httpRequestV1_1P = do
  -- request line
  m <- httpMethodP
  void space
  rt <- requestTargetP
  void space
  void . ABC.string $ renderHTTPVersion HTTP_1_1
  void crlf
  -- FIXME: HOST header
  hs <- fmap (HTTPRequestHeaders . NE.fromList) $ AB.sepBy1' headerP crlf
  void crlf
  void crlf
  payload <- do
    e <- AB.atEnd
    if e
      then pure NoRequestBody
      else fmap RequestBody AB.takeByteString
  pure . HTTPV1_1Request $ HTTPRequestV1_1 m rt hs payload
  where
    space = AB.word8 0x20

    crlf = AB.word8 0x0d >> AB.word8 0x0a

httpMethodP :: Parser HTTPMethod
httpMethodP = HTTPMethod <$> AB.takeWhile isTokenWord
