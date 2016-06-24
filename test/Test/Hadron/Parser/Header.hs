{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Parser.Header where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Hadron.Data.Header
import           Hadron.Parser.Header

import           Test.Hadron.Arbitrary ()
import           Test.Hadron.Parser

prop_tripping_HeaderName = trippingP renderHeaderName headerNameP

prop_tripping_HeaderValue = trippingP renderHeaderValue headerValueP

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
