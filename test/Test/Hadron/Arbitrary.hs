{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hadron.Arbitrary where

import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import           P

import           Hadron.Data

import           Test.Hadron.Gen
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (NonEmpty a)
  where
    arbitrary =
      NE.fromList <$> listOf1 arbitrary

instance Arbitrary HTTPVersion where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary HTTPMethod where
  arbitrary = genHTTPMethod

instance Arbitrary HeaderName where
  arbitrary = genHeaderName

  shrink = genericShrink

-- Tabs are allowed, but not as the initial character.
instance Arbitrary HeaderValue where
  arbitrary = genHeaderValue

  shrink = genericShrink

instance Arbitrary Header where
  arbitrary = genHeader

  shrink = genericShrink

instance Arbitrary URIPath where
  arbitrary = genURIPath

  shrink (URIPath "/") = []
  shrink x = filter (not . BS.null . unURIPath) $ (URIPath "/") : shrink x

instance Arbitrary QueryString where
  arbitrary = genQueryString

  shrink = genericShrink

instance Arbitrary Fragment where
  arbitrary = genFragment

  shrink = genericShrink

instance Arbitrary RequestTarget where
  arbitrary = genRequestTarget

  shrink = genericShrink

instance Arbitrary HTTPRequestHeaders where
  arbitrary = genHTTPRequestHeaders

  shrink = genericShrink

instance Arbitrary RequestBody where
  arbitrary = genRequestBody

  shrink NoRequestBody = []
  shrink rb =
    let bss' = divide rb in
    NoRequestBody : bss'
    where
      divide NoRequestBody = []
      divide (RequestBody x) = if BS.null x
        then []
        else
          let x' = RequestBody $ BS.take ((BS.length x) `div` 2) x in
          [x'] <> divide x'

instance Arbitrary HTTPRequestV1_1 where
  arbitrary = genHTTPRequestV1_1

  shrink = genericShrink

instance Arbitrary HTTPRequest where
  arbitrary = genHTTPRequest

  shrink = genericShrink
