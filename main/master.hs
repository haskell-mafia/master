{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import           BuildInfo_master

import           Data.String (String)

import           Options.Applicative

import           P

import           System.IO
import           System.Exit (exitFailure, exitSuccess)
import           System.Environment (getArgs)

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

--- FIX These should all go into optparse-extra ---

command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  command label (info (parser <**> helper) (progDesc description))

dispatch :: Parser a -> IO a
dispatch p = getArgs >>= \x -> case x of
  [] -> customExecParser (prefs showHelpOnError)  (info (p <**> helper) idm)
  _  -> execParser (info (p <**> helper) idm)
