{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Parser.Request(
    httpMethodP
  ) where

import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AB

import           Hadron.Data.Request
import           Hadron.Parser.Common

import           P

httpMethodP :: Parser HTTPMethod
httpMethodP = HTTPMethod <$> AB.takeWhile isTokenWord
