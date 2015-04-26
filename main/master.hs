{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BuildInfo_master

import           Options.Applicative

import           P

import           System.IO
import           System.Exit (exitFailure, exitSuccess)

import           X.Options.Applicative


data Command =
    VersionCommand
  | BuildCommand
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch master >>= \sc ->
    case sc of
      VersionCommand ->
        (putStrLn $ "master: " <> buildInfoVersion) >> exitSuccess
      BuildCommand ->
        putStrLn "not-implemented" >> exitFailure


master :: Parser Command
master =
  versionP <|> commandP

commandP :: Parser Command
commandP =  subparser $
     command' "build"
              "Build project"
              (pure BuildCommand)

versionP :: Parser Command
versionP =
  flag' VersionCommand $
       short 'v'
    <> long "version"
    <> help "Display the version for the master executable."
