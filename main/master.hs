{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BuildInfo_ambiata_master

import           Data.Text (pack)

import           Options.Applicative

import           Master

import           P

import           System.IO
import           System.Exit (exitFailure, exitSuccess)

import           X.Options.Applicative

data Command =
  BuildCommand (Maybe JobName) deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch (safeCommand commandP) >>= \sc -> case sc of
    VersionCommand ->
      (putStrLn $ "master: " <> buildInfoVersion) >> exitSuccess
    RunCommand DryRun c ->
      print c >> exitFailure
    RunCommand RealRun (BuildCommand _) ->
      putStrLn "not-implemented" >> exitFailure


commandP :: Parser Command
commandP =  subparser $
  command' "build"
           "Build project"
           (BuildCommand <$> jobP)

jobP :: Parser (Maybe JobName)
jobP = optional $ (JobName . pack) <$> (strArgument $
     metavar "JOB"
  <> help "Job name.")
