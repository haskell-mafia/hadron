{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Parser.Common(
    isTokenWord
  , isVisible
  ) where

import           Data.Word

import           P

-- | Valid part of a "token" as defined by RFC 7230.
--
-- Any visible ASCII character except a delimiter.
isTokenWord :: Word8 -> Bool
isTokenWord 0x21 = True -- exclamation mark
isTokenWord 0x23 = True -- hash
isTokenWord 0x24 = True -- dollar
isTokenWord 0x25 = True -- percent
isTokenWord 0x26 = True -- ampersand
isTokenWord 0x27 = True -- single quote
isTokenWord 0x2a = True -- asterisk
isTokenWord 0x2b = True -- plus
isTokenWord 0x2d = True -- hyphen
isTokenWord 0x2e = True -- period
isTokenWord 0x5e = True -- caret
isTokenWord 0x5f = True -- underscore
isTokenWord 0x60 = True -- backtick
isTokenWord 0x7c = True -- pipe
isTokenWord 0x7e = True -- tilde
isTokenWord w = or [
    (w >= 0x41 && w <= 0x5a) -- A-Z
  , (w >= 0x61 && w <= 0x7a) -- a-z
  , (w >= 0x30 && w <= 0x39) -- 0-9
  ]

-- | Printable ASCII characters.
isVisible :: Word8 -> Bool
isVisible w = w >= 0x20 && w < 0x7f
