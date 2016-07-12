{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Hadron.Core.Gen where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (ord)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T

import           Disorder.Corpus (viruses)

import           Hadron.Core.Data

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import qualified Test.QuickCheck.Utf8 as QT

import           Text.Printf (printf)

data EmptyAllowed =
    EmptyAllowed
  | EmptyForbidden

genStdHttpMethod :: Gen ByteString
genStdHttpMethod = elements [
    "GET"
  , "HEAD"
  , "POST"
  , "PUT"
  , "DELETE"
  , "CONNECT"
  , "OPTIONS"
  , "TRACE"
  ]

byteStringOf g ea = do
  fmap BS.pack $ v g
  where
    v = case ea of
      EmptyAllowed -> listOf
      EmptyForbidden -> listOf1

genAlphaNumWord = oneof [
    choose (0x41, 0x5a) -- upper
  , choose (0x61, 0x7a) -- lower
  , choose (0x30, 0x39) -- digits
  ]

genTokenWord = oneof [
    genAlphaNumWord
  , elements $ toWords "!#$%&'*+-.^_`|~"
  ]

genToken = byteStringOf genTokenWord

genVisibleWord = choose (0x20, 0x7e)

genVisible = byteStringOf genVisibleWord

genURIUnreservedWord = frequency [(9, genAlphaNumWord), (1, punctuation)]
  where
    punctuation = elements $ toWords "-._~"

toWords = fmap (fromIntegral . ord)

genURISubDelimWord = elements $ toWords "!$&'()*+,;="

genURIPcharExtraWord = elements $ toWords ":@"

genPercentEncoded = do
  x <- oneof [QT.oneByte, QT.twoByte, QT.threeByte]
  pure $ percentEncode x

percentEncode = BSC.pack . concatMap (printf "%%%02x") . BS.unpack

genURIPchar = oneof [
    fmap BS.singleton genURIUnreservedWord
  , fmap BS.singleton genURISubDelimWord
  , fmap BS.singleton genURIPcharExtraWord
  , genPercentEncoded
  ]

genURISegment ea = do
  n <- choose (lb, 100)
  fmap BS.concat $ vectorOf n genURIPchar
  where
    lb = case ea of
      EmptyAllowed -> 0
      EmptyForbidden -> 1

-- Full path-absolute form from RFC 3986.
genURIPath = fmap URIPath $
  frequency [(1, emptyPath), (999, nonEmptyPath)]
  where
    emptyPath = pure "/"

    nonEmptyPath = do
      s0 <- genURISegment EmptyForbidden
      ss <- listOf (genURISegment EmptyAllowed)
      pure $ BS.concat [
          "/"
        , s0
        , BS.intercalate "/" ss
        ]

genQueryStringFragmentPart =
  frequency [(1, genExtra), (99, genURIPchar)]
  where
    genExtra = fmap BS.singleton $ elements $ toWords "/?"

genHostHeader = do
  hv <- fmap HeaderValue $ genVisible EmptyForbidden
  pure $ Header (HeaderName "host") (pure hv)

genHeaderValue = fmap HeaderValue $ oneof [
    genVisible EmptyAllowed
  , genVisibleWithTab
  ]
  where
    genVisibleWithTab = liftM2 (<>) (genVisible EmptyForbidden) $ oneof [
        genVisible EmptyForbidden
      , fmap ("\t" <>) genVisibleWithTab
      , liftM2 (<>) (genVisible EmptyAllowed) genVisibleWithTab
      ]

genHeaderName = HeaderName <$> genToken EmptyForbidden

genHeader = Header <$> genHeaderName <*> (fmap NE.fromList $ listOf1 genHeaderValue)

genHTTPMethod = fmap HTTPMethod $ oneof [
    genStdHttpMethod
  , genToken EmptyForbidden
  ]

genQueryStringWithValues = do
  n <- choose (1, 10)
  is <- vectorOf n qsItem
  pure . QueryStringPart . BS.intercalate "&" $ renderItem <$> is
  where
    qsItem = (,) <$> genQSBit <*> genQSBit

    renderItem (k, v) =
      k <> "=" <> v

    genQSBit = frequency [
        (1, fmap BS.concat (listOf1 genQueryStringFragmentPart))
      , (9, fmap T.encodeUtf8 (elements viruses))
      ]

genQueryString = frequency [(1, pure NoQueryString), (999, genQueryString')]
  where
    genQueryString' = do
      ps <- fmap BS.concat $ listOf genQueryStringFragmentPart
      pure $ QueryStringPart ps

genFragment = frequency [(1, pure NoFragment), (999, genFragment')]
  where
    genFragment' = do
      ps <- fmap BS.concat $ listOf genQueryStringFragmentPart
      pure $ FragmentPart ps

genRequestTarget =  oneof [
    AbsPathTarget <$> genURIPath <*> genQueryString <*> genFragment
  ]

genHTTPRequestHeaders = do
  hostH <- genHostHeader
  hs <- listOf genHeader
  pure . HTTPRequestHeaders $ hostH :| hs

genRequestBody = frequency [
    (1, pure NoRequestBody)
  , (999, bsBody)
  ]
  where
    bsBody = fmap RequestBody $ do
      n <- choose (1, 100)
      fmap BS.pack . vectorOf n $ choose (0, 255)

genHTTPRequestV1_1 =
  HTTPRequestV1_1
    <$> genHTTPMethod
    <*> genRequestTarget
    <*> genHTTPRequestHeaders
    <*> genRequestBody

genHTTPRequest =
  genHTTPRequest' genHTTPMethod genRequestTarget genHTTPRequestHeaders genRequestBody

genHTTPRequest' gm grt grh grb = oneof [
    fmap HTTPV1_1Request $
      HTTPRequestV1_1
        <$> gm
        <*> grt
        <*> grh
        <*> grb
  ]

data RequestMalformation =
    MalformedNoPath
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary RequestMalformation where
  arbitrary = elements [minBound..maxBound]

genMalformedRequest' MalformedNoPath = genMalformedNoPath

genMalformedNoPath =
  fmap renderHTTPRequest $ genHTTPRequest' genHTTPMethod malformedTarget genHTTPRequestHeaders genRequestBody
  where
    malformedTarget = do
      qs <- genQueryString
      f <- genFragment
      pure $ AbsPathTarget (URIPath "") qs f
