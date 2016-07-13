{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Core.Parser.Header where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Hadron.Core.Data.Header
import           Hadron.Core.Parser.Header

import           Test.Hadron.Core.Arbitrary ()
import           Test.Hadron.Core.Parser

prop_tripping_HeaderName = trippingP renderHeaderName headerNameP

prop_tripping_HeaderValue = trippingP renderHeaderValue headerValueP

prop_tripping_Header = trippingP renderHeader headerP

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
