{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Hadron.Wai.Request where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Hadron.Core
import           Hadron.Wai.Request

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

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
