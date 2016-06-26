{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Hadron.Parser.Header(
    headerNameP
  , headerValueP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AB

import           Hadron.Data.Header
import           Hadron.Parser.Common

import           P

headerNameP :: Parser HeaderName
headerNameP =
  HeaderName <$> AB.takeWhile isTokenWord

-- | We don't need to separate out multiple header values here, we can
-- do that on render if we need to.
headerValueP :: Parser HeaderValue
headerValueP =
  fmap HeaderValue $ AB.peekWord8 >>= \case
    Nothing -> pure "" -- empty header values are technically valid
    Just w ->
      if isVisible w
        then AB.takeWhile isHeaderWord
        else fail "invalid initial header value character"
  where
    isHeaderWord 0x09 = True -- tab
    isHeaderWord w = isVisible w

