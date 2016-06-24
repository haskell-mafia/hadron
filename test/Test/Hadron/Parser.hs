{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Parser where

import qualified Data.Attoparsec.ByteString as AB

import           Disorder.Core.Tripping (tripping)

import           P

trippingP to from = tripping to (AB.parseOnly (from <* AB.endOfInput))
