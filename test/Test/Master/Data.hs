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
  masterJobSelect (Just n) (MasterConfig mr $ M.singleton n mj { masterJobRunner = Just mjr } <> mjs)
    === Just (mjr, masterJobParams mj)

prop_masterJobSelect_no_job n mj mjs mr =
  masterJobSelect (Just n) (MasterConfig mr $ M.singleton n mj { masterJobRunner = Nothing } <> mjs)
    === Just (mr, masterJobParams mj)

prop_masterJobSelect_no_name =
  isJust . masterJobSelect Nothing

prop_masterJobSelect_invalid_name n mc = M.notMember n (masterJobs mc) ==>
  masterJobSelect (Just n) mc === Nothing


return []
tests :: IO Bool
tests = $quickCheckAll
