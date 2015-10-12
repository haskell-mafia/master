{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Master.Runner where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.Either.Combinators (mapLeft)
import           Data.Text as T

import           Disorder.Core.IO
import           Disorder.Corpus

import           Master as M
import           Mismi.S3 as S3

import           P

import           System.IO
import           System.IO.Temp
import qualified System.Directory as D
import qualified System.FilePath as F

import           Test.Mismi.Amazonka
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_get_file_missing = testIO . withSystemTempDirectory "missing" $ \d -> do
  x <- runEitherT (getFile d (RunnerPath d))
  pure $ (mapLeft renderRunnerError x) === (mapLeft renderRunnerError (Left $ MissingFile d))

prop_get_file_local t = testIO . withSystemTempDirectory "missing" $ \d -> do
  let out = d F.</> "master"
  writeFile out t
  r <- runEitherT $ getFile d (RunnerPath out)
  z <- getFilePath r
  pure $ z === out

prop_get_file_s3_no_hash = forAll (elements weather) $ \s -> withLocalAWS $ \d a -> do
  liftIO $ D.createDirectoryIfMissing True d
  S3.writeOrFail a s
  r <- liftIO . runEitherT $ getFile d (RunnerS3 a Nothing)
  z <- getValue r
  pure $ z === s

prop_download = forAll (elements southpark) $ \s -> withLocalAWS $ \d a -> do
  liftIO $ D.createDirectoryIfMissing True d
  S3.writeOrFail a s
  r <- liftIO . runEitherT $ M.download d a
  z <- getValue r
  pure $ z === s

prop_download_os = forAll (elements southpark) $ \s -> withLocalAWS $ \d a -> do
  liftIO $ D.createDirectoryIfMissing True d
  -- Write out the two values we know are supported on our dev machines or on the build
  S3.writeOrFail (withKey (</> Key "darwin-x86_64") a) s
  S3.writeOrFail (withKey (</> Key "linux-x86_64") a) s
  r <- liftIO . runEitherT $ M.download d $ withKey (</> Key "$OS-$ARCH") a
  z <- getValue r
  pure $ z === s

getFilePath  :: Monad m => Either RunnerError a -> m a
getFilePath r = case r of
  Left e ->
    fail . T.unpack $ renderRunnerError e
  Right o' ->
    return o'

getValue :: (Applicative m, MonadIO m) => Either RunnerError FilePath -> m Text
getValue r = do
  o <- getFilePath r
  liftIO $ readFile o >>= (pure . pack)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
