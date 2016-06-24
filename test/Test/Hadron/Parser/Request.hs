{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Parser.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Hadron.Data.Request
import           Hadron.Parser.Request

import           Test.Hadron.Arbitrary ()
import           Test.Hadron.Parser

prop_tripping_HTTPMethod = trippingP renderHTTPMethod httpMethodP

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
