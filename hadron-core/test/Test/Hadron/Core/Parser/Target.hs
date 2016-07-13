{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Core.Parser.Target where

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as BS

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Hadron.Core.Data.Target
import           Hadron.Core.Parser.Target

import           Test.Hadron.Core.Arbitrary ()
import           Test.Hadron.Core.Gen
import           Test.Hadron.Core.Parser
import           Test.QuickCheck ((===), forAll, choose)

prop_URIPath_tripping = trippingP renderURIPath uriPathP

prop_QueryString_tripping = trippingP renderQueryString queryStringP

prop_Fragment_tripping = trippingP renderFragment fragmentP

prop_RequestTarget_tripping = trippingP renderRequestTarget requestTargetP

prop_percentEncodedP = forAll (fmap BS.singleton $ choose (0, 255)) $ \bs ->
  let bs' = percentEncode bs
      r = AB.parseOnly percentEncodedP bs' in
  r === Right bs'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
