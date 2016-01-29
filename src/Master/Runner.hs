{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Master.Runner (
    RunnerError (..)
  , runner
  , getFile
  , download
  , renderRunnerError
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import "cryptohash" Crypto.Hash as H

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import           Data.Text
import           Data.Text.Encoding
import qualified Data.Text as T
import           Data.UUID
import           Data.UUID.V4


import           Master.Data

import           Mismi
import           Mismi.S3 (Address, DownloadError)
import qualified Mismi.S3 as S3

import           P

import           System.Directory
import           System.Environment
import           System.IO
import           System.Posix.Process
import           System.FilePath

import           X.Control.Monad.Trans.Either (EitherT, left, bimapEitherT, firstEitherT)

-- root ~= "~/.master/cache"
runner :: FilePath -> MasterRunner -> MasterJobParams -> EitherT RunnerError IO ()
runner root run env = do
  f <- getFile root run
  lift $ exec f env

getFile :: FilePath -> MasterRunner -> EitherT RunnerError IO FilePath
getFile root mr = case mr of
  RunnerPath f ->
    ifM (lift $ doesFileExist f) (pure f) . left $ MissingFile f

  RunnerS3 add (Just v) -> do
    let f = root </> (T.unpack v)
    ifM (lift $ doesFileExist f) (pure f) $ download root add

  RunnerS3 add Nothing ->
    download root add

download :: FilePath -> Address -> EitherT RunnerError IO FilePath
download root addr = do
  env <- bimapEitherT AwsRegionError id discoverAWSEnv
  uuid <- liftIO nextRandom >>= return . toString
  let f = root </> "master" <.> uuid
  runAWST env (AwsError addr) . firstEitherT (DownloadError addr) $
    S3.download addr f
  bs <- liftIO $ LBS.readFile f
  let sha = H.digestToHexByteString $ (H.hashlazy bs :: Digest SHA1)
      out = root </> (T.unpack $ decodeUtf8 sha)
  liftIO $ createDirectoryIfMissing True root
  p <- liftIO $ getPermissions f
  liftIO . setPermissions f $ setOwnerExecutable True p
  liftIO $ renameFile f out
  pure out

exec :: FilePath -> MasterJobParams -> IO a
exec cmd m = do
  e <- getEnvironment
  let env = e <> fmap (bimap unpack unpack) (M.toList m)
  executeFile cmd False [] $ Just env

data RunnerError =
    MissingFile FilePath
  | AwsError Address Error
  | DownloadError Address DownloadError
  | AwsRegionError RegionError
  deriving (Show)

renderRunnerError :: RunnerError -> Text
renderRunnerError r = case r of
  MissingFile f ->
    "RunnerLocal [" <> T.pack f <> "] does not exists."
  AwsError a e ->
    "Downloading runner [" <> S3.addressToText a <> "] failed - " <> renderError e
  DownloadError a e ->
    "Downloading runner [" <> S3.addressToText a <> "] failed - " <> S3.renderDownloadError e
  AwsRegionError e ->
    "Failed to retrieve environment: " <> renderRegionError e
