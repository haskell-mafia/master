{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Master.Data where

import qualified Data.Map as M

import           Master.Data

import           P

import           System.IO

import           Test.Master.Arbitrary ()
import           Test.QuickCheck


prop_masterJobSelect_job n mj mjr mjs mr =
  masterJobSelect (Just n) (MasterConfig mr $ M.singleton n mj { masterJobRunner = mjr } <> mjs)
    === Just (mjr, masterJobParams mj)

prop_masterJobSelect_no_job mjs mr =
  masterJobSelect Nothing (MasterConfig (Just mr) mjs)
    === Just (mr, M.empty)

prop_masterJobSelect_invalid_name n mc = M.notMember n (masterJobs mc) ==>
  masterJobSelect (Just n) mc === Nothing


return []
tests :: IO Bool
tests = $quickCheckAll
