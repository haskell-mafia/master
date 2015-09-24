{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BuildInfo_ambiata_master

import           Control.Monad.Trans.Either

import           Data.Map as M
import           Data.Text as T

import           Master

import           Options.Applicative

import           P

import           System.Directory
import           System.FilePath
import           System.Environment
import           System.Exit (exitSuccess)
import           System.IO

import           X.Options.Applicative

data Command =
    BuildCommand (Maybe FilePath) (Maybe JobName)
  | ListCommand (Maybe FilePath)
  deriving (Eq, Show)

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
    RunCommand rt (BuildCommand mf mjn) -> do
      mc <- orDie masterLoadErrorRender . EitherT $ loadMasterConfig mf
      (mr, mp) <- orDie id . hoistEither . maybeToRight "Master build not found" $ masterJobSelect mjn mc
      case rt of
        DryRun ->
          putStrLn . T.unpack
            $ "Found runner [" <> masterRunnerRender mr <> "] with parameters [" <> masterJobParamsRender mp <> "]"
        RealRun ->
          orDie renderRunnerError $ runner cache mr mp
    RunCommand _ (ListCommand mf) -> do
      mc <- orDie masterLoadErrorRender . EitherT $ loadMasterConfig mf
      traverse_ (putStrLn . T.unpack . jobName . fst) . M.toList $ masterJobs mc

commandP :: Parser Command
commandP = subparser $ buildP <> listP

buildP :: Mod CommandFields Command
buildP =
  command' "build"
           "Build project"
           (BuildCommand <$> fileP <*> jobP)

-- FIX Should probably have verbose mode to print out params and/or runner (or you know just look at the file)
listP :: Mod CommandFields Command
listP =
  command' "list"
           "List the builds for this project"
           (ListCommand <$> fileP)

jobP :: Parser (Maybe JobName)
jobP = optional $ (JobName . T.pack) <$> (strArgument $
     metavar "JOB"
  <> help "Job name.")

fileP :: Parser (Maybe FilePath)
fileP = optional . strOption $
     metavar "MASTER_FILE"
  <> short 'f'
  <> long "file"
  <> help "An optional path to a master file, otherwise defaults to 'master.toml'"
