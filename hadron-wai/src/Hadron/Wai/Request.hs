{-# LANGUAGE NoImplicitPrelude #-}
module Hadron.Wai.Request(
    toHTTPRequest
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.CaseInsensitive as CI
import           Data.List.NonEmpty (nonEmpty)

import           Hadron.Core
import qualified Hadron.Core.Parser.Header as H
import qualified Hadron.Core.Parser.Request as H
import qualified Hadron.Core.Parser.Target as H
import           Hadron.Wai.Error

import qualified Network.HTTP.Types.Version as HT
import qualified Network.Wai as W

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT)
import           X.Control.Monad.Trans.Either (hoistEither, left)

toHTTPRequest :: W.Request -> EitherT WaiRequestError IO HTTPRequest
toHTTPRequest r = do
  case W.httpVersion r of
    (HT.HttpVersion 1 1) -> toHTTPRequest_1_1 r
    (HT.HttpVersion major minor) -> left $ WaiUnsupportedHTTPVersion major minor

toHTTPRequest_1_1 :: W.Request -> EitherT WaiRequestError IO HTTPRequest
toHTTPRequest_1_1 r = do
  m <- parse' WaiInvalidRequestMethod H.httpMethodP $ W.requestMethod r
  b <- liftIO $ RequestBody <$> W.requestBody r
  t <- parse' WaiInvalidRequestTarget H.requestTargetP $ W.rawPathInfo r
  hs <- hadronRequestHeaders $ W.requestHeaders r
  pure . HTTPV1_1Request $ HTTPRequestV1_1 m t hs b
  where
    parse' e p bs = case AB.parseOnly p bs of
      Left _ -> left e
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
