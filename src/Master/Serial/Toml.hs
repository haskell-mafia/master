{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Serial.Toml (
    MasterLoadError (..)
  , MasterConfig' (..)
  , MasterJob (..)
  , loadMasterConfigToml
  , masterJobSelect
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


-- | An intermediate representation just for parsing the master configuration
data MasterConfig' =
  MasterConfig' {
    masterRunner' :: Maybe MasterRunner
  , masterJobs :: M.Map JobName MasterJob
  } deriving (Eq, Show)

data MasterJob =
  MasterJob {
    masterJobRunner :: Maybe MasterRunner
  , masterJobParams :: MasterJobParams
  } deriving (Eq, Show)

data MasterLoadError =
    MasterParseError FilePath ParseError
  | MasterFromError MasterFromError
  deriving (Eq, Show)

data MasterFromError =
    MissingJob (Maybe JobName)
  | InvalidNodeType Text
  | UnknownVersion Int64
  | MissingVersion
  deriving (Eq, Show)


masterJobSelect :: Maybe JobName -> MasterConfig' -> Maybe MasterConfig
masterJobSelect mjn (MasterConfig' mr mjs) =
  uncurry MasterConfig <$> maybe (flip (,) M.empty <$> mr) (\jn ->
    (\(MasterJob mr' mj) -> flip (,) mj <$> (mr' <|> mr)) =<< M.lookup jn mjs
    ) mjn

loadMasterConfigToml :: FilePath -> Maybe JobName -> IO (Either MasterLoadError MasterConfig)
loadMasterConfigToml fp jn = do
  t <- T.readFile fp
  pure
    . either
      (Left . MasterParseError fp)
      (first MasterFromError . (=<<) (maybeToRight (MissingJob jn) . masterJobSelect jn) . masterConfigFromToml)
    $ parseTomlDoc fp t

masterConfigFromToml :: Table -> Either MasterFromError MasterConfig'
masterConfigFromToml t' = do
  (m, t) <- splitMasterFromBuild t'
  case HM.lookup "version" m of
    Just (NTValue (VInteger 1)) ->
      MasterConfig'
        <$> masterRunnerFromToml m
        <*> masterJobsFromToml t
    Just (NTValue (VInteger v)) ->
      Left $ UnknownVersion v
    _ ->
      Left $ MissingVersion

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

masterConfigToToml :: MasterConfig' -> Table
masterConfigToToml (MasterConfig' r j) =
  (HM.singleton masterKey . NTable . (versionTable <>) . maybe HM.empty masterRunnerToToml) r <>
    HM.singleton "build" (NTable . HM.fromList . fmap (bimap jobName (NTable . masterJobToToml)) . M.toList $ j)

masterRunnerToToml :: MasterRunner -> Table
masterRunnerToToml = HM.fromList . \case
  RunnerPath v ->
    pure ("runner", vstring $ T.pack v)
  RunnerS3 a h ->
    ("runner", vstring $ addressToText a) : (maybeToList . fmap ((,) "sha" . vstring)) h
  where
    vstring = NTValue . VString

versionTable :: Table
versionTable = HM.singleton "version" . NTValue $ VInteger currentVersion

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
  MissingJob Nothing -> "Master build not specified and no default found"
  MissingJob (Just m) -> "Master build '" <> jobName m <> "' not found"
  InvalidNodeType t -> "The TOML type of '"  <> t <> "' is invalid, must be a string"
  UnknownVersion v -> "The master.version '" <> (T.pack . show) v <> "' is not supported'"
  MissingVersion -> "The master.version attribute is mandatory - the latest version is '" <> (T.pack . show) currentVersion <> "'"

currentVersion :: Int64
currentVersion = 1

masterKey :: Text
masterKey = "master"
