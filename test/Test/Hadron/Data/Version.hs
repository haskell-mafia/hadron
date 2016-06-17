{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Hadron.Data.Version where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Hadron.Data.Version

import           Test.Hadron.Arbitrary ()
import           Test.QuickCheck (Property)

prop_tripping_HTTPVersion :: HTTPVersion -> Property
prop_tripping_HTTPVersion = tripping renderHTTPVersion parseHTTPVersion

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
