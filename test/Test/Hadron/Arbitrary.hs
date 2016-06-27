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
  arbitrary = fmap HTTPMethod $ oneof [
      genStdHttpMethod
    , genToken EmptyForbidden
    ]

instance Arbitrary HeaderName where
  arbitrary = HeaderName <$> genToken EmptyForbidden

  shrink = genericShrink

-- Tabs are allowed, but not as the initial character.
instance Arbitrary HeaderValue where
  arbitrary = fmap HeaderValue $ oneof [
      genVisible EmptyAllowed
    , genVisibleWithTab
    ]
    where
      genVisibleWithTab = liftM2 (<>) (genVisible EmptyForbidden) $ oneof [
          genVisible EmptyForbidden
        , fmap ("\t" <>) genVisibleWithTab
        , liftM2 (<>) (genVisible EmptyAllowed) genVisibleWithTab
        ]

  shrink = genericShrink

instance Arbitrary Header where
  arbitrary = Header <$> arbitrary <*> arbitrary

  shrink = genericShrink

instance Arbitrary URIPath where
  arbitrary = fmap URIPath genURIPath

  shrink (URIPath "/") = []
  shrink x = filter (not . BS.null . unURIPath) $ (URIPath "/") : shrink x

-- FIXME: have a more realistic example as well as the "everything we're
-- allowed to do" version
instance Arbitrary QueryString where
  arbitrary = frequency [(1, pure NoQueryString), (999, genQueryString')]
    where
      genQueryString' = do
        ps <- fmap BS.concat $ listOf genQueryStringFragmentPart
        pure $ QueryStringPart ps

  shrink = genericShrink

instance Arbitrary Fragment where
  arbitrary = frequency [(1, pure NoFragment), (999, genFragment')]
    where
      genFragment' = do
        ps <- fmap BS.concat $ listOf genQueryStringFragmentPart
        pure $ FragmentPart ps

  shrink = genericShrink

instance Arbitrary RequestTarget where
  arbitrary = oneof [
      AbsPathTarget <$> arbitrary <*> arbitrary <*> arbitrary
    ]

  shrink = genericShrink

instance Arbitrary HTTPRequestHeaders where
  arbitrary = do
    hostH <- genHostHeader
    hs <- listOf arbitrary
    pure . HTTPRequestHeaders $ hostH :| hs

  shrink = genericShrink

instance Arbitrary RequestBody where
  arbitrary = frequency [
      (1, pure NoRequestBody)
    , (999, bsBody)
    ]
    where
      bsBody = fmap RequestBody $ do
        n <- choose (1, 100)
        fmap BS.pack . vectorOf n $ choose (0, 255)

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
  arbitrary =
    HTTPRequestV1_1
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

  shrink = genericShrink

instance Arbitrary HTTPRequest where
  arbitrary = oneof [
      HTTPV1_1Request <$> arbitrary
    ]

  shrink = genericShrink
