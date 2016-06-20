{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hadron.Arbitrary where

import           P

import           Hadron.Data

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary HTTPVersion where
  arbitrary = elements [minBound..maxBound]

