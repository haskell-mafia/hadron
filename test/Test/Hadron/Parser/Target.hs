{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Parser.Target where

import qualified Data.Attoparsec.ByteString as AB

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Hadron.Data.Target
import           Hadron.Parser.Target

import           Test.Hadron.Arbitrary ()
import           Test.Hadron.Gen
import           Test.Hadron.Parser
import           Test.QuickCheck ((===))

prop_URIPath_tripping = trippingP renderURIPath uriPathP

prop_QueryString_tripping = trippingP renderQueryString queryStringP

prop_Fragment_tripping = trippingP renderFragment fragmentP

prop_percentEncodedP w =
  let bs = percentEncode w
      r = AB.parseOnly percentEncodedP bs in
  r === Right bs

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
