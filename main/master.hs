{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BuildInfo_ambiata_master

import           Control.Monad.Trans.Either

import           Data.Text (pack)

import           Master

import           Options.Applicative

import           P
import qualified Prelude as PE

import           System.Directory
import           System.FilePath
import           System.Environment
import           System.Exit (exitFailure, exitSuccess)
import           System.IO

import           X.Options.Applicative

data Command =
  BuildCommand (Maybe JobName) deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  m <- lookupEnv "MASTER_CACHE"
  h <- getHomeDirectory
  let cache = maybe (h </> ".master" </> "cache") id m
  dispatch (safeCommand commandP) >>= \sc -> case sc of
    VersionCommand ->
      (putStrLn $ "master: " <> buildInfoVersion) >> exitSuccess
    RunCommand DryRun c ->
      print c >> exitFailure
    RunCommand RealRun (BuildCommand _) -> do
      _ <- orDie masterLoadErrorRender . EitherT $ loadMasterConfigToml "master.toml"
      orDie renderRunnerError $ runner cache PE.undefined PE.undefined

commandP :: Parser Command
commandP =  subparser $
  command' "build"
           "Build project"
           (BuildCommand <$> jobP)

jobP :: Parser (Maybe JobName)
jobP = optional $ (JobName . pack) <$> (strArgument $
     metavar "JOB"
  <> help "Job name.")
