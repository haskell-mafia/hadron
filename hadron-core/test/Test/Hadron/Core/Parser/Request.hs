{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Core.Parser.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Hadron.Core.Data.Request
import           Hadron.Core.Parser.Request

import           Test.Hadron.Core.Arbitrary ()
import           Test.Hadron.Core.Parser

prop_tripping_HTTPMethod = trippingP renderHTTPMethod httpMethodP

prop_tripping_HTTPRequest = trippingP renderHTTPRequest httpRequestP

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
