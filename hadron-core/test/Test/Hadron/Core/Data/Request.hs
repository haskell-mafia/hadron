{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Core.Data.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Hadron.Core.Data
import           Hadron.Core.Header

import           P

import qualified Prelude

import           System.IO (IO)

import           Test.Hadron.Core.Arbitrary ()
import           Test.QuickCheck

prop_requestHeaders_pos (HTTPRequestHeaders hs) =
  requestHeaders hs === Just' (HTTPRequestHeaders hs)

prop_requestHeaders_neg hs =
  not (any ((== hostHeaderName) . httpHeaderName) hs) ==>
    requestHeaders hs === Nothing'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
