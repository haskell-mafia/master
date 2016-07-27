{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Master.Serial.Toml where

import           Data.List as L
import           Data.Map as M

import           Disorder.Core.Tripping

import           Master.Data
import           Master.Serial.Toml

import           P

import           System.IO

import           Test.Master.Arbitrary ()
import           Test.QuickCheck


prop_parse =
  tripping masterConfigToToml masterConfigFromToml

prop_masterJobSelect_job n mj mjr mjs mr =
  masterJobSelect (Just n) (MasterConfig' mr M.empty $ M.singleton n mj { masterJobRunner = Just mjr } <> mjs)
    === Just (MasterConfig mjr (masterJobParams mj))

prop_masterJobSelect_no_job n mj mjs mr =
  masterJobSelect (Just n) (MasterConfig' (Just mr) M.empty $ M.singleton n mj { masterJobRunner = Nothing } <> mjs)
    === Just (MasterConfig mr (masterJobParams mj))

prop_masterJobSelect_no_name mc mr =
  isJust . masterJobSelect Nothing $ mc { masterRunner' = Just mr }

prop_masterJobSelect_invalid_name n mc = M.notMember n (masterJobs mc) ==>
  masterJobSelect (Just n) mc === Nothing

prop_masterJobSelect_globals_override n mr mjr (Positive nvals) env =
  forAll (vectorOf nvals arbitrary) $ \ks ->
    forAll (vectorOf nvals arbitrary) $ \gvals ->
      forAll (vectorOf nvals arbitrary) $ \lvals ->
        let gmap = M.fromList (L.zip ks gvals)
            lmap = M.fromList (L.zip ks lvals)
        in masterJobSelect (Just n) (MasterConfig' mr (gmap <> env) (M.singleton n (MasterJob (Just mjr) lmap)))
           === Just (MasterConfig mjr (lmap <> env))

prop_masterJobSelect_globals n mr mjr (Positive nvals) env =
  forAll (vectorOf nvals arbitrary) $ \ks ->
    forAll (vectorOf nvals arbitrary) $ \gvals ->
      let gmap = M.fromList (L.zip ks gvals)
      in masterJobSelect (Just n) (MasterConfig' mr gmap (M.singleton n (MasterJob (Just mjr) env)))
         === Just (MasterConfig mjr (env <> gmap))


return []
tests :: IO Bool
tests = $quickCheckAll
