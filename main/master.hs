{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
import           BuildInfo_ambiata_master

import           Data.Text as T

import           Master

import           Options.Applicative

import           P

import           System.Directory
import           System.FilePath
import           System.Environment
import           System.Exit (exitSuccess, exitFailure)
import           System.Posix.Directory (changeWorkingDirectory)
import           System.IO

import           X.Control.Monad.Trans.Either (pattern EitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative

data Command =
    BuildCommand (Maybe FilePath) (Maybe FilePath) JobName
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
    DependencyCommand ->
      -- TODO make this meaningful
      mapM putStrLn [[]] >> exitSuccess
    RunCommand rt (BuildCommand dir mf jn) -> do
      for_ dir $ \d -> do
        unlessM (doesDirectoryExist d) $ do
          hPutStrLn stderr "Specified directory does not exist, can not change working directory."
          exitFailure
        changeWorkingDirectory d
      MasterConfig mr mp <- orDie masterLoadErrorRender . EitherT $ loadMasterConfig mf jn
      case rt of
        DryRun ->
          putStrLn . T.unpack
            $ "Found runner [" <> masterRunnerRender mr <> "] with parameters [" <> masterJobParamsRender mp <> "]"
        RealRun ->
          orDie renderRunnerError $ runner cache mr mp

commandP :: Parser Command
commandP = subparser buildP

buildP :: Mod CommandFields Command
buildP =
  command' "build"
           "Build project"
           (BuildCommand <$> dirP <*> fileP <*> jobP)

jobP :: Parser JobName
jobP = (JobName . T.pack) <$> (strArgument $
     metavar "JOB"
  <> help "Job name.")

fileP :: Parser (Maybe FilePath)
fileP = optional . strOption $
     metavar "MASTER_FILE"
  <> short 'f'
  <> long "file"
  <> help "An optional path to a master file, otherwise defaults to 'master.toml'"

dirP :: Parser (Maybe FilePath)
dirP = optional . strOption $
     metavar "WORKING_DIRECTORY"
  <> short 'C'
  <> long "directory"
  <> help "Change to specified directory before reading master file or executing build."
