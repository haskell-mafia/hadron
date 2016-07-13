{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Data.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Hadron.Data
import           Hadron.Header

import           P

import qualified Prelude

import           System.IO (IO)

import           Test.Hadron.Arbitrary ()
import           Test.QuickCheck

prop_requestHeaders_pos (HTTPRequestHeaders hs) =
  requestHeaders hs === Just' (HTTPRequestHeaders hs)

prop_requestHeaders_neg hs =
  not (any ((== hostHeaderName) . httpHeaderName) hs) ==>
    requestHeaders hs === Nothing'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
