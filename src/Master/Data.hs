{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Data (
    MasterConfig (..)
  , MasterRunner (..)
  , MasterExecutable (..)
  , JobName (..)
  , MasterJobParams
  , Hash (..)
  , masterRunnerRender
  , masterJobParamsRender
  , globalJobParams
  ) where

import           Data.Map as M
import           Data.Text as T

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
  , masterParams :: MasterJobParams
  } deriving (Eq, Show)

type MasterJobParams = Map Text Text

newtype Hash =
  Hash {
    renderHash :: Text
  } deriving (Eq, Show)

data MasterRunner =
    RunnerS3 Address (Maybe Hash)
  | RunnerPath FilePath
  deriving (Eq, Show)


masterRunnerRender :: MasterRunner -> Text
masterRunnerRender = \case
  RunnerS3 a _ -> addressToText a
  RunnerPath p -> T.pack p

masterJobParamsRender :: MasterJobParams -> Text
masterJobParamsRender =
  T.intercalate "," . fmap (\(a, b) -> a <> "=" <> b) . M.toList

globalJobParams :: JobName -> MasterJobParams -> MasterJobParams
globalJobParams jn jobparams =
  let
    globals = M.fromList $ [
        ("MASTER_BUILD", jobName jn)
      ]
  in
    M.union jobparams globals
