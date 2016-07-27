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
  , hashFile
  , hashText
  , hashLBS
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
    ifM (lift $ doesFileExist f) (validate f v *> pure f) $ download root add (Just v)

  RunnerS3 add Nothing -> do
    liftIO $ hPutStrLn stderr "warning: using an S3 runner without a hash"
    download root add Nothing

download :: FilePath -> Address -> Maybe Hash -> EitherT RunnerError IO FilePath
download root addr mhash = do
  env <- bimapEitherT AwsRegionError id discoverAWSEnv
  uuid <- liftIO nextRandom >>= return . toString
  let f = root </> "master" <.> uuid
  runAWST env (AwsError addr) . firstEitherT (DownloadError addr) $
    S3.download addr f
  install root f mhash

install :: FilePath -> FilePath -> Maybe Hash -> EitherT RunnerError IO FilePath
install root f Nothing = do
  sha <- liftIO (hashFile f)
  relocate root f sha
install root f (Just v) = do
  validate f v
  relocate root f v

-- Move into the cache according to hash and chmod. Does not check the hash.
relocate :: FilePath -> FilePath -> Hash -> EitherT RunnerError IO FilePath
relocate root f sha = do
  let out = root </> (T.unpack sha)
  liftIO $ createDirectoryIfMissing True root
  p <- liftIO $ getPermissions f
  liftIO . setPermissions f $ setOwnerExecutable True p
  liftIO $ renameFile f out
  pure out

validate :: FilePath -> Hash -> EitherT RunnerError IO ()
validate f h = do
  sha <- liftIO $ hashFile f
  unless (sha == h) (left (BadHash h sha))

hashFile :: FilePath -> IO Hash
hashFile f = do
  bs <- LBS.readFile f
  pure (hashLBS bs)

hashText :: Text -> Hash
hashText =
  hashLBS . LBS.fromStrict . encodeUtf8

hashLBS :: LBS.ByteString -> Hash
hashLBS bs =
  decodeUtf8 . H.digestToHexByteString $ (H.hashlazy bs :: Digest SHA1)

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
  | BadHash Hash Hash
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
  BadHash e a ->
    "Failed to validate hash: expected " <> e <> ", got " <> a
