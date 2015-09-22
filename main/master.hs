{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BuildInfo_ambiata_master

import           Options.Applicative

import           P

import           System.IO
import           System.Exit (exitFailure, exitSuccess)

import           X.Options.Applicative


data Command =
    BuildCommand

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch (safeCommand commandP) >>= \sc ->
    case sc of
      VersionCommand ->
        (putStrLn $ "master: " <> buildInfoVersion) >> exitSuccess
      RunCommand _ BuildCommand ->
        putStrLn "not-implemented" >> exitFailure


commandP :: Parser Command
commandP =  subparser $
     command' "build"
              "Build project"
              (pure BuildCommand)
