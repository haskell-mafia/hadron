{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Hadron.Wai.Request(
    toHTTPRequest
  , fromHTTPRequest
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.IORef as I
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE

import           Hadron.Core
import qualified Hadron.Core.Parser.Header as H
import qualified Hadron.Core.Parser.Request as H
import qualified Hadron.Core.Parser.Target as H
import           Hadron.Wai.Error

import qualified Network.HTTP.Types as HT
import qualified Network.Wai as W

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT)
import           X.Control.Monad.Trans.Either (hoistEither, left)

-- | Convert a wai Request object to a hadron HTTPRequest object, failing in
-- case of an invalid wai request or an unsupported HTTP version.
--
-- This conversion is not lossless, as wai request objects contain information
-- such as transport type (HTTP/HTTPS) which is not part of the HTTP protocol
-- layer.
--
-- Achtung: as hadron does not currently support streaming request bodies,
-- this will read the entire payload into memory regardless of how it is
-- chunked inside wai. wai's requestBody, strictRequestBody and
-- lazyRequestBody functions will return an empty result for the
-- request after this.
toHTTPRequest :: W.Request -> EitherT WaiRequestError IO HTTPRequest
toHTTPRequest r = do
  case W.httpVersion r of
    (HT.HttpVersion 1 1) -> toHTTPRequest_1_1 r
    (HT.HttpVersion major minor) -> left $ WaiUnsupportedHTTPVersion major minor

toHTTPRequest_1_1 :: W.Request -> EitherT WaiRequestError IO HTTPRequest
toHTTPRequest_1_1 r = do
  m <- parse' WaiInvalidRequestMethod H.httpMethodP $ W.requestMethod r
  b <- liftIO . fmap (RequestBody . BSL.toStrict) $ W.strictRequestBody r
  t <- parse' WaiInvalidRequestTarget H.requestTargetP $ W.rawPathInfo r <> W.rawQueryString r
  hs <- hadronRequestHeaders $ W.requestHeaders r
  pure . HTTPV1_1Request $ HTTPRequestV1_1 m t hs b
  where
    parse' e p bs = case AB.parseOnly p bs of
      Left _ -> left $ e bs
      Right x -> pure x

    hadronRequestHeaders hs = case nonEmpty hs of
      Nothing -> left WaiNoRequestHeaders
      Just hs' -> do
        hs'' <- mapM fromWaiHeader hs'
        maybe' (left WaiNoHostHeader) pure $ requestHeaders hs''

    fromWaiHeader (hn, hv) = firstEitherT (const WaiInvalidRequestHeaders) . hoistEither $ do
      hn' <- AB.parseOnly H.headerNameP $ CI.original hn
      hv' <- AB.parseOnly H.headerValueP hv
      pure $ Header hn' (pure hv')

-- | Convert a hadron HTTPRequest object into a wai Request.
fromHTTPRequest :: HTTPRequest -> IO W.Request
fromHTTPRequest (HTTPV1_1Request r) = fromHTTPRequest_1_1 r

fromHTTPRequest_1_1 :: HTTPRequestV1_1 -> IO W.Request
fromHTTPRequest_1_1 (HTTPRequestV1_1 m t h b) =
  let
    wBodyLen = W.KnownLength . fromIntegral . BS.length $ renderRequestBody b
  in do
  wBody <- buildBody b
  pure $ W.defaultRequest {
      W.httpVersion = HT.http11
    , W.requestMethod = unHTTPMethod m
    , W.rawPathInfo = renderRequestTarget t
    , W.requestHeaders = buildHeaders h
    , W.requestBody = wBody
    , W.requestBodyLength = wBodyLen
    }
  where
    -- Construct a wai request body generator. This is an IO action which
    -- constructs another IO action; the IORef is there so we return the
    -- single body chunk and then an empty string to terminate thereafter.
    buildBody :: RequestBody -> IO (IO ByteString)
    buildBody NoRequestBody =
      pure $ pure ""
    buildBody (RequestBody bs) = do
      alreadyRead <- I.newIORef False
      pure . I.atomicModifyIORef' alreadyRead $ \fp -> case fp of
        True -> (True, "")
        False -> (True, bs)

    buildHeaders (HTTPRequestHeaders hs) =
      NE.toList $ buildHeader <$> hs

    buildHeader (Header hn hvs) =
      let hn' = CI.mk $ renderHeaderName hn
          hvs' = BS.intercalate "," . NE.toList $ unHeaderValue <$> hvs in
      (hn', hvs')
