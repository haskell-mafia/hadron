{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Hadron.Header(
    authorizationHeaderName
  ) where

import           Hadron.Data.Header

authorizationHeaderName :: HeaderName
authorizationHeaderName = HeaderName "authorization"
