{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Master.Arbitrary where

import           Data.Map as M

import           Master.Data
import           Master.Serial.Toml

import           P

import           Test.Mismi.S3.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary MasterConfig where
  arbitrary = MasterConfig
    <$> arbitrary
    <*> arbitrary

instance Arbitrary MasterConfig' where
  arbitrary = do
    r <- arbitrary
    -- Ensure we always have a runner
    oneof [
        MasterConfig'
          <$> pure (Just r)
          <*> arbitrary
          <*> arbitrary
      , MasterConfig'
          <$> pure Nothing
          <*> arbitrary
          <*> (fmap M.fromList $ listOf1 (fmap (\(MasterJob r' p) -> MasterJob (Just $ fromMaybe r r') p) <$> arbitrary))
      ]

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
