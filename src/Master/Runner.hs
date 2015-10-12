{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Master.Runner (
    RunnerError (..)
  , runner
  , getFile
  , download
  , renderRunnerError
  ) where

import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import "cryptohash" Crypto.Hash as H

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import           Data.Text
import           Data.Text.Encoding
import qualified Data.Text as T

import           Master.Data

import           Mismi
import           Mismi.S3 (Address (..), Key (..))
import qualified Mismi.S3 as S3

import           P

import           System.Directory
import           System.Environment
import           System.IO
import           System.IO.Temp
import           System.Posix.Process
import           System.Posix.Unistd (getSystemID, systemName, machine)
import           System.FilePath


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
download root (Address b (Key k)) = do
  addr <- liftIO . fmap (Address b . Key) $ replacePlatform k
  env <- bimapEitherT AwsRegionError id discoverAWSEnv
  bimapEitherT (AwsError addr) id . runAWS env $ withSystemTempDirectory "master" $ \d -> do
    let f = d </> "master"
    S3.download addr f
    bs <- liftIO $ LBS.readFile f
    let sha = H.digestToHexByteString $ (H.hashlazy bs :: Digest SHA1)
        out = root </> (T.unpack $ decodeUtf8 sha)
    liftIO $ renameFile f $ out
    pure out

exec :: FilePath -> MasterJobParams -> IO a
exec cmd m = do
  e <- getEnvironment
  let env = e <> fmap (bimap unpack unpack) (M.toList m)
  executeFile cmd False [] $ Just env

replacePlatform :: Text -> IO Text
replacePlatform t = do
  sid <- getSystemID
  pure
    . T.replace "$ARCH" (T.pack $ machine sid)
    . T.replace "$OS" (T.toLower . T.pack $ systemName sid)
    $ t


data RunnerError =
    MissingFile FilePath
  | AwsError Address Error
  | AwsRegionError RegionError
  deriving (Show)

renderRunnerError :: RunnerError -> Text
renderRunnerError r = case r of
  MissingFile f ->
    "RunnerLocal [" <> T.pack f <> "] does not exists."
  AwsError a e ->
    "Downloading runner [" <> S3.addressToText a <> "] failed - " <> errorRender e
  AwsRegionError e ->
    "Failed to retrieve environment: " <> renderRegionError e
