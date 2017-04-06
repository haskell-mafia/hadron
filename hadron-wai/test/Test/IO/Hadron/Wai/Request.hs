{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Hadron.Wai.Request where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IORef as I
import           Data.List (replicate)

import           Disorder.Core.Gen (utf8BS1)
import           Disorder.Core.IO (testIO)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Hadron.Core
import           Hadron.Wai.Request

import qualified Network.HTTP.Types as HT
import qualified Network.Wai as W

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.Hadron.Core.Arbitrary ()

import           X.Control.Monad.Trans.Either (runEitherT)

prop_tripping_HTTPRequest :: HTTPRequest -> Property
prop_tripping_HTTPRequest hr = testIO $ do
  wr <- fromHTTPRequest hr
  hr' <- runEitherT $ toHTTPRequest wr
  pure $ hr' === Right hr

prop_pathInfo_HTTPRequest_v1 :: HTTPV1_1Request -> Property
prop_pathInfo_HTTPRequest_v1 hr =
  testIO $ do
    wr <- fromHTTPRequest (HTTPV1_1Request hr)
    pure $
      (BSL.toStrict . BS.toLazyByteString . HT.encodePathSegments . W.pathInfo) wr
      ===
      (renderRequestTarget . hrqv1_1Target) hr

-- Make sure we convert wai requests with multi-chunk bodies correctly.
prop_tripping_HTTPRequest_chunked :: HTTPRequest -> Property
prop_tripping_HTTPRequest_chunked hr = forAll (choose (2, 20)) $ \n -> forAll utf8BS1 $ \bs -> testIO $ do
  wr <- fromHTTPRequest hr
  b <- chunkedBody n bs
  let wr' = wr { W.requestBody = b }
  let hr'' = hr `withBody` (RequestBody . BS.concat $ replicate n bs)
  hr' <- runEitherT $ toHTTPRequest wr'
  pure $ hr' === Right hr''

withBody :: HTTPRequest -> RequestBody -> HTTPRequest
withBody (HTTPV1_1Request r) b =
  HTTPV1_1Request $ r { hrqv1_1Body = b }

chunkedBody :: Int -> ByteString -> IO (IO ByteString)
chunkedBody n bs = do
  chunksRead <- I.newIORef 0
  pure . I.atomicModifyIORef' chunksRead $ \cr -> case cr == n of
    True -> (n, "")
    False -> (cr + 1, bs)

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
