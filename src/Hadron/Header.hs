{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Header(
    authorizationHeaderName
  , hostHeaderName
  ) where

import           Hadron.Data.Header

authorizationHeaderName :: HeaderName
authorizationHeaderName = HeaderName "authorization"

hostHeaderName :: HeaderName
hostHeaderName = HeaderName "host"

