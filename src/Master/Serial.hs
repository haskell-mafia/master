{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Serial (
    module X
  , loadMasterConfig
  ) where

import           Master.Data
import           Master.Serial.Toml as X

import           P

import           System.IO

loadMasterConfig :: Maybe FilePath -> JobName -> IO (Either MasterLoadError MasterConfig)
loadMasterConfig fp jn =
  flip (fmap . fmap) (loadMasterConfigToml (fromMaybe "master.toml" fp) jn) $ \(MasterConfig r c) ->
    MasterConfig r $ globalJobParams jn c
