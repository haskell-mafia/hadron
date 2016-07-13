{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Hadron.Core.Data.Version where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Hadron.Core.Data.Version

import           Test.Hadron.Core.Arbitrary ()
import           Test.QuickCheck (Property)

prop_tripping_HTTPVersion :: HTTPVersion -> Property
prop_tripping_HTTPVersion = tripping renderHTTPVersion parseHTTPVersion

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
