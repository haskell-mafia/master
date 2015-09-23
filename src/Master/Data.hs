{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Data (
    MasterConfig (..)
  , MasterJob (..)
  , MasterRunner (..)
  , MasterExecutable (..)
  , JobName (..)
  , MasterJobParams
  , Hash
  ) where

import           Data.Map
import           Data.Text

import           Mismi.S3

import           P

import           System.FilePath

newtype JobName =
  JobName {
      jobName :: Text
    } deriving (Eq, Ord, Show)

newtype MasterExecutable =
  MasterExecutable {
      executable :: FilePath
    } deriving (Eq, Show)

data MasterConfig =
  MasterConfig {
    masterRunner :: MasterRunner
  , masterJobs :: Map JobName MasterJob
  } deriving (Eq, Show)

data MasterJob =
  MasterJob {
    masterJobRunner :: Maybe MasterRunner
  , masterJobParams :: MasterJobParams
  } deriving (Eq, Show)

type MasterJobParams = Map Text Text
type Hash = Text

data MasterRunner =
    RunnerS3 Address (Maybe Hash)
  | RunnerPath FilePath
  deriving (Eq, Show)
