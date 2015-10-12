{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Serial.Toml (
    MasterLoadError (..)
  , loadMasterConfigToml
  , masterConfigFromToml
  , masterConfigToToml
  , masterLoadErrorRender
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Data.Text as T
import           Data.Text.IO as T

import           Master.Data

import           Mismi.S3.Data

import           P

import           System.IO

import           Text.Parsec.Error
import           Text.Toml
import           Text.Toml.Types


data MasterLoadError =
    MasterParseError FilePath ParseError
  | MasterFromError MasterFromError
  deriving (Eq, Show)

data MasterFromError =
    MissingRunner
  | InvalidNodeType Text
  deriving (Eq, Show)


loadMasterConfigToml :: FilePath -> IO (Either MasterLoadError MasterConfig)
loadMasterConfigToml fp = do
  t <- T.readFile fp
  pure
    . either (Left . MasterParseError fp) (first MasterFromError . masterConfigFromToml)
    $ parseTomlDoc fp t

masterConfigFromToml :: Table -> Either MasterFromError MasterConfig
masterConfigFromToml t' = do
  (m, t) <- splitMasterFromBuild t'
  MasterConfig
    <$> ((=<<) (maybeToRight MissingRunner) . masterRunnerFromToml) m
    <*> masterJobsFromToml t

splitMasterFromBuild :: Table -> Either MasterFromError (Table, Table)
splitMasterFromBuild t =
  flip (,) (HM.delete masterKey t) <$> case HM.lookup masterKey t of
    Just (NTable m) -> Right m
    Just _ -> Left $ InvalidNodeType masterKey
    _ -> Right HM.empty

masterRunnerFromToml :: Table -> Either MasterFromError (Maybe MasterRunner)
masterRunnerFromToml t = do
  for (HM.lookup "runner" t) $ \case
    NTValue (VString v) ->
      case addressFromText v of
        Nothing ->
          pure . RunnerPath $ T.unpack v
        Just a -> do
          h <- case HM.lookup "sha" $ t of
            Nothing -> pure Nothing
            Just (NTValue (VString s)) -> pure $ Just s
            Just _ -> Left $ InvalidNodeType "sha"
          pure $ RunnerS3 a h
    _ ->
      Left $ InvalidNodeType "runner"

masterJobsFromToml :: Table -> Either MasterFromError (M.Map JobName MasterJob)
masterJobsFromToml t = do
  case HM.lookupDefault (NTable HM.empty) "build" t of
    NTable bt' ->
      fmap (M.fromList . fmap (first JobName)) . for (HM.toList bt') $ \(k, v) -> (,) k <$> case v of
        NTable bt'' -> do
          (m, bt) <- splitMasterFromBuild bt''
          MasterJob
            <$> masterRunnerFromToml m
            <*> masterJobFromToml bt
        _ ->
          Left . InvalidNodeType $ "build." <> k
    _ ->
      Left $ InvalidNodeType "build"

masterJobFromToml :: Table -> Either MasterFromError MasterJobParams
masterJobFromToml t = do
  fmap M.fromList . for (HM.toList t) $ \(k, v) -> case v of
    (NTValue (VString v')) -> Right (k, v')
    _ -> Left $ InvalidNodeType k

masterConfigToToml :: MasterConfig -> Table
masterConfigToToml (MasterConfig r j) =
  HM.singleton masterKey (NTable $ masterRunnerToToml r) <>
    HM.singleton "build" (NTable . HM.fromList . fmap (bimap jobName (NTable . masterJobToToml)) . M.toList $ j)

masterRunnerToToml :: MasterRunner -> Table
masterRunnerToToml = HM.fromList . \case
  RunnerPath v ->
    pure ("runner", vstring $ T.pack v)
  RunnerS3 a h ->
    ("runner", vstring $ addressToText a) : (maybeToList . fmap ((,) "sha" . vstring)) h
  where
    vstring = NTValue . VString

masterJobToToml :: MasterJob -> Table
masterJobToToml (MasterJob r p) =
     maybe HM.empty (HM.singleton masterKey . NTable . masterRunnerToToml) r
  <> (HM.fromList . fmap (second vstring) . M.toList) p
  where
    vstring = NTValue . VString

masterLoadErrorRender :: MasterLoadError -> Text
masterLoadErrorRender = \case
  MasterParseError fp e -> T.pack $ "Could not parse " <> fp <> ": " <> show e
  MasterFromError e -> masterFromErrorRender e

masterFromErrorRender :: MasterFromError -> Text
masterFromErrorRender = \case
  MissingRunner -> "The 'runner' field is mandatory"
  InvalidNodeType t -> "The TOML type of '"  <> t <> "' is invalid, must be a string"

masterKey :: Text
masterKey = "master"
