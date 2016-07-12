{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Hadron.Data
import           Hadron.Header
import           Hadron.Request

import           P

import qualified Prelude

import           System.IO (IO)

import           Test.Hadron.Arbitrary ()
import           Test.QuickCheck

prop_add_remove_requestHeader r@(HTTPV1_1Request r11) hn hvs =
  let hns = fmap httpHeaderName . unHTTPRequestHeaders $ hrqv1_1Headers r11
      r' = addRequestHeader r (Header hn hvs)
      r'' = removeRequestHeader r' hn in
  not (elem hn hns) ==>
    r'' === Just' r

prop_add_remove_lookup_requestHeader r@(HTTPV1_1Request r11) hn hvs =
  let hns = fmap httpHeaderName . unHTTPRequestHeaders $ hrqv1_1Headers r11
      res1 = lookupRequestHeader r hn
      r' = addRequestHeader r (Header hn hvs)
      res2 = lookupRequestHeader r' hn
      r'' = fromJust' $ removeRequestHeader r' hn
      res3 = lookupRequestHeader r'' hn in
  not (elem hn hns) ==>
    (res1, res2, res3) === (Nothing', Just' hvs, Nothing')
  where
    fromJust' (Just' x) = x
    fromJust' Nothing' = Prelude.error "unexpected Nothing'"

prop_requestHeaders_pos (HTTPRequestHeaders hs) =
  requestHeaders hs === Just' (HTTPRequestHeaders hs)

prop_requestHeaders_neg hs =
  not (any ((== hostHeaderName) . httpHeaderName) hs) ==>
    requestHeaders hs === Nothing'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
