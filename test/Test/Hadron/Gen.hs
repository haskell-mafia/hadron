{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Gen where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

data EmptyAllowed =
    EmptyAllowed
  | EmptyForbidden

genStdHttpMethod :: Gen ByteString
genStdHttpMethod = elements [
    "GET"
  , "HEAD"
  , "POST"
  , "PUT"
  , "DELETE"
  , "CONNECT"
  , "OPTIONS"
  , "TRACE"
  ]

byteStringOf g ea = do
  n <- choose (lb, 100)
  fmap BS.pack $ vectorOf n g
  where
    lb = case ea of
      EmptyAllowed -> 0
      EmptyForbidden -> 1

genAlphaNumWord = oneof [
    choose (0x41, 0x5a) -- upper
  , choose (0x61, 0x7a) -- lower
  , choose (0x30, 0x39) -- digits
  ]

genTokenWord = oneof [
    genAlphaNumWord
  , elements tokens
  ]
  where
    tokens = [
        0x21
      , 0x23
      , 0x24
      , 0x25
      , 0x26
      , 0x27
      , 0x2a
      , 0x2b
      , 0x2d
      , 0x2e
      , 0x5e
      , 0x5f
      , 0x60
      , 0x7c
      , 0x7e
      ]

genToken = byteStringOf genTokenWord

genVisibleWord = choose (0x20, 0x7e)

genVisible = byteStringOf genVisibleWord
