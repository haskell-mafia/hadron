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

instance Arbitrary Header where
  arbitrary = Header <$> arbitrary <*> arbitrary

instance Arbitrary URIPath where
  arbitrary = fmap URIPath genURIPath

-- FIXME: have a more realistic example as well as the "everything we're
-- allowed to do" version
instance Arbitrary QueryString where
  arbitrary = frequency [(1, pure NoQueryString), (999, genQueryString')]
    where
      genQueryString' = do
        ps <- fmap BS.concat $ listOf genQueryStringFragmentPart
        pure $ QueryStringPart ps

instance Arbitrary Fragment where
  arbitrary = frequency [(1, pure NoFragment), (999, genFragment')]
    where
      genFragment' = do
        ps <- fmap BS.concat $ listOf genQueryStringFragmentPart
        pure $ FragmentPart ps

instance Arbitrary RequestTarget where
  arbitrary = oneof [
      AbsPathTarget <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary HTTPRequestHeaders where
  arbitrary = do
    hostH <- genHostHeader
    hs <- listOf arbitrary
    pure . HTTPRequestHeaders $ hostH :| hs

instance Arbitrary RequestBody where
  arbitrary = frequency [
      (1, pure NoRequestBody)
    , (999, bsBody)
    ]
    where
      bsBody = fmap RequestBody $ do
        n <- choose (1, 100)
        fmap BS.pack . vectorOf n $ choose (0, 255)

instance Arbitrary HTTPRequestV1_1 where
  arbitrary =
    HTTPRequestV1_1
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary HTTPRequest where
  arbitrary = oneof [
      HTTPV1_1Request <$> arbitrary
    ]
