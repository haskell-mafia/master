{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Data (
    MasterConfig (..)
  , MasterRunner (..)
  , MasterExecutable (..)
  , JobName (..)
  , MasterJobParams
  , Hash
  , masterRunnerParse
  , masterRunnerRender
  , masterJobParamsRender
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
type Hash = Text

data MasterRunner =
    RunnerS3 Address (Maybe Hash)
  | RunnerPath FilePath
  deriving (Eq, Show)


masterRunnerRender :: MasterRunner -> Text
masterRunnerRender = \case
  RunnerS3 a s -> addressToText a <> (" (" <> maybe "no hash" ("SHA: " <>) s <> ")")
  RunnerPath p -> T.pack p

masterRunnerParse :: Text -> Maybe Hash -> MasterRunner
masterRunnerParse r h = case addressFromText r of
  Nothing -> RunnerPath $ T.unpack r
  Just r' -> RunnerS3 r' h

masterJobParamsRender :: MasterJobParams -> Text
masterJobParamsRender =
  T.intercalate "," . fmap (\(a, b) -> a <> "=" <> b) . M.toList
