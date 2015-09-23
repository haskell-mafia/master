{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Master.Serial.Toml where

import           Disorder.Core.Tripping

import           Master.Serial.Toml

import           P

import           System.IO

import           Test.Master.Arbitrary ()
import           Test.QuickCheck


prop_parse =
  tripping masterConfigToToml masterConfigFromToml


return []
tests :: IO Bool
tests = $quickCheckAll
