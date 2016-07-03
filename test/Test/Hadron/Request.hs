{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Hadron.Data
import           Hadron.Request

import           Test.Hadron.Arbitrary ()
import           Test.QuickCheck

prop_add_lookup_requestHeader r@(HTTPV1_1Request r11) hn hvs =
   let hns = fmap httpHeaderName . unHTTPRequestHeaders $ hrqv1_1Headers r11
       res1 = lookupRequestHeader r hn
       r' = addRequestHeader r (Header hn hvs)
       res2 = lookupRequestHeader r' hn in
   not (elem hn hns) ==>
     (res1, res2) === (Nothing', Just' hvs)

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
