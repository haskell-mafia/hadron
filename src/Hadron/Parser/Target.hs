{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Hadron.Parser.Target(
    uriPathP
  , percentEncodedP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word (Word8)

import           Hadron.Data.Target
import           Hadron.Parser.Common

import           P

import           X.Data.Attoparsec.ByteString (sepByByte1)
import           X.Data.Attoparsec.ByteString.Ascii (isAlphaNum)

-- | We're parsing the path-absolute form in RFC 3986:
--
-- "/" [ segment-nz *( "/" segment) ]
--
-- This parser is pretty slow, could make it faster by rewriting as a
-- take-while-predicate with a stateful scanner for percent-encoding.
uriPathP :: Parser URIPath
uriPathP = do
  void $ AB.word8 slash
  AB.peekWord8 >>= \case
    Nothing -> pure $ URIPath "/"
    Just 0x2f -> fail "Initial URI segment starts with //"
    Just _ -> do
      s0 <- segmentNZ
      ss <- sepByByte1 segment slash
      pure . URIPath $ BS.concat [
          "/"
        , s0
        , BS.intercalate "/" ss
        ]
  where
    slash = 0x2f
  
    segmentNZ = pchar

    segment = fmap BS.concat $ many pchar

    pchar = AB.choice [
        uriUnreservedP
      , percentEncodedP
      , uriSubDelimP
      , pcharExtraP
      ]

    pcharExtraP = fmap BS.singleton $ AB.word8 0x3a <|> AB.word8 0x40 -- : or @

-- | unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
uriUnreservedP :: Parser ByteString
uriUnreservedP = fmap BS.singleton $ AB.satisfy isURIUnreserved

isURIUnreserved :: Word8 -> Bool
isURIUnreserved 0x2d = True -- hyphen
isURIUnreserved 0x2e = True -- period
isURIUnreserved 0x5f = True -- underscore
isURIUnreserved 0x7e = True -- tilde
isURIUnreserved 0x40 = False -- at
isURIUnreserved w = isAlphaNum w

-- | sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
--                  / "*" / "+" / "," / ";" / "="
uriSubDelimP :: Parser ByteString
uriSubDelimP = fmap BS.singleton $ AB.satisfy isURISubDelim

isURISubDelim :: Word8 -> Bool
isURISubDelim 0x21 = True -- exclamation
isURISubDelim 0x24 = True -- dollar
isURISubDelim 0x26 = True -- ampersand
isURISubDelim 0x27 = True -- single-quote
isURISubDelim 0x28 = True -- left paren
isURISubDelim 0x29 = True -- right paren
isURISubDelim 0x2a = True -- asterisk
isURISubDelim 0x2b = True -- plus
isURISubDelim 0x2c = True -- comma
isURISubDelim 0x3b = True -- semicolon
isURISubDelim 0x3d = True -- equals
isURISubDelim _ = False

-- | pct-encoded = "%" HEXDIG HEXDIG
--
-- In URIs, these are UTF-8 values, but we don't currently attempt to validate
-- them beyond ensuring they're valid hex.
percentEncodedP :: Parser ByteString
percentEncodedP = do
  void $ AB.word8 0x25 -- %
  b <- hexByte
  pure $ "%" <> b
