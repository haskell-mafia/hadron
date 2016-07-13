{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Core.Error(
    RequestError(..)
  , renderRequestError
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

data RequestError =
    RequestParsingError !Text
  deriving (Eq, Show, Generic)

instance NFData RequestError where rnf = genericRnf

renderRequestError :: RequestError -> Text
renderRequestError (RequestParsingError e) = "error parsing request: " <> e
