{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Master.Arbitrary where

import           Master.Data

import           P

import           Test.Mismi.S3.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary MasterConfig where
  arbitrary = MasterConfig
    <$> arbitrary
    <*> arbitrary

instance Arbitrary MasterJob where
  arbitrary = MasterJob
    <$> arbitrary
    <*> arbitrary

instance Arbitrary JobName where
  arbitrary = JobName <$> arbitrary

instance Arbitrary MasterRunner where
  arbitrary = oneof [
      RunnerPath <$> arbitrary
    , RunnerS3 <$> arbitrary <*> arbitrary
    ]
