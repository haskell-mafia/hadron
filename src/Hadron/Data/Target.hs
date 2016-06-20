{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Hadron.Data.Target(
    RequestTarget(..)
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           GHC.Generics (Generic)

import           P

-- | A 'RequestTarget' is derived from but not always the same as the
-- URI the client is requesting.
--
-- The syntax/semantics of a 'RequestTarget' is specified in RFC
-- 7230 (section 5.3) with reference to RFC 3986. 'RequestTarget'
-- implements the subset of URI forms used in HTTP. 
--
-- FIXME: implement these.
data RequestTarget =
    AsteriskTarget
  | AbsoluteTarget
  | AbsPathTarget
  | AuthorityTarget
  deriving (Eq, Show, Generic)

instance NFData RequestTarget where rnf = genericRnf
