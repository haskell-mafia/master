{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Master.Runner where

import           Control.Monad.IO.Class

import           Data.Text as T

import           Disorder.Core.IO
import           Disorder.Corpus

import           Master
import qualified Mismi.S3 as S3

import           P

import           System.IO
import           System.IO.Temp
import qualified System.FilePath as F

import           Test.Mismi (testAWS)
import           Test.Mismi.S3 (newFilePath, newAddress)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either (runEitherT)

prop_get_file_missing = testIO . withSystemTempDirectory "missing" $ \d -> do
  x <- runEitherT (getFile d (RunnerPath d))
  pure $ (first renderRunnerError x) === (first renderRunnerError (Left $ MissingFile d))

prop_get_file_local t = testIO . withSystemTempDirectory "missing" $ \d -> do
  let out = d F.</> "master"
  writeFile out t
  r <- runEitherT $ getFile d (RunnerPath out)
  z <- getFilePath r
  pure $ z === out

prop_get_file_s3_no_hash = forAll (elements weather) $ \s -> testAWS $ do
  d <- newFilePath
  a <- newAddress
  S3.writeOrFail a s
  r <- liftIO . runEitherT $ getFile d (RunnerS3 a Nothing)
  z <- getValue r
  pure $ z === s

prop_download = forAll (elements southpark) $ \s -> testAWS $ do
  d <- newFilePath
  a <- newAddress
  S3.writeOrFail a s
  r <- liftIO . runEitherT $ download d a Nothing
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
