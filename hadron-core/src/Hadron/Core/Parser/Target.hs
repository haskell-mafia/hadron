{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Hadron.Core.Parser.Target(
    absPathTargetP
  , queryStringP
  , fragmentP
  , percentEncodedP
  , requestTargetP
  , uriPathP
  ) where

import           Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word (Word8)

import           Hadron.Core.Data.Target
import           Hadron.Core.Parser.Common

import           P

import           X.Data.Attoparsec.ByteString (sepByByte1)
import           X.Data.Attoparsec.ByteString.Ascii (isAlphaNum)

-- | Target part of the request - second element of the request-line after the
-- method and before the HTTP version.
requestTargetP :: Parser RequestTarget
requestTargetP = absPathTargetP -- and theoretically other options later

-- | Absolute path URI target, e.g., "/example/foo/bar.html".
absPathTargetP :: Parser RequestTarget
absPathTargetP = do
  p <- uriPathP <?> "uriPathP"
  qs <- queryStringP <?> "queryStringP"
  f <- fragmentP <?> "fragmentP"
  pure $ AbsPathTarget p qs f

fragmentP :: Parser Fragment
fragmentP =
  -- URI fragment must start with a hash. If we have a space there's no
  -- fragment part. If we have anything else we have an invalid URI.
  AB.peekWord8 >>= \case
    Nothing -> pure NoFragment -- end of input
    Just 0x20 -> pure NoFragment -- space
    Just 0x23 -> fmap FragmentPart fragmentP' -- hash
    Just _ -> fail "fragment does not begin with #"
  where
    fragmentP' =
      void (AB.word8 0x23) >> queryStringFragmentP

queryStringP :: Parser QueryString
queryStringP =
  -- Query string part must start with a question mark. If we see a hash
  -- instead, we don't have a query string but we do have a fragment.
  -- If we see space we're at the end of the URI.
  -- If we see any other character we have an invalid URI.
  AB.peekWord8 >>= \case
    Nothing -> pure NoQueryString -- end of input
    Just 0x23 -> pure NoQueryString -- hash
    Just 0x20 -> pure NoQueryString -- space
    Just 0x3f -> fmap QueryStringPart queryStringP' -- question mark
    Just _ -> fail "query string does not begin with ?"
  where
    queryStringP' =
      void (AB.word8 0x3f) >> queryStringFragmentP

-- | The tail of the query-string or fragment section.
--
-- The query-string part is terminated by a # or space. The fragment
-- part is terminated by space.
queryStringFragmentP :: Parser ByteString
queryStringFragmentP =
  fmap BS.concat $ AB.many' part
  where
    part = uriPcharP <|> (fmap BS.singleton $ AB.satisfy extra)

    extra 0x2f = True -- /
    extra 0x3f = True -- ?
    extra _ = False

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
    Just _ -> nonEmptyPath <|> (pure $ URIPath "/")
  where
    slash = 0x2f

    segmentNZ = uriPcharP

    segment = fmap BS.concat $ many uriPcharP

    nonEmptyPath = do
      s0 <- segmentNZ
      ss <- sepByByte1 segment slash
      pure . URIPath $ BS.concat [
          "/"
        , s0
        , BS.intercalate "/" ss
        ]


uriPcharP :: Parser ByteString
uriPcharP = AB.choice [
    uriUnreservedP
  , percentEncodedP
  , uriSubDelimP
  , pcharExtraP
  ]
  where
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
