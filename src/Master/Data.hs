{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Data (
    MasterConfig (..)
  , MasterJob (..)
  , MasterRunner (..)
  , JobName (..)
  ) where

import           Data.Text

import           Mismi.S3

import           P

import           System.IO


newtype JobName =
  JobName {
      jobName :: Text
    } deriving (Eq, Show)

data MasterConfig =
  MasterConfig {
    masterRunner :: MasterRunner
  , masterJobs :: [MasterJob]
  } deriving (Eq, Show)

data MasterJob =
  MasterJob {
    masterJobName :: JobName
  , masterJobRunner :: Maybe MasterRunner
  , masterJobParams :: [(Text, Text)]
  } deriving (Eq, Show)

type Hash = Text

data MasterRunner =
    RunnerS3 Address (Maybe Hash)
  | RunnerPath FilePath
  deriving (Eq, Show)
