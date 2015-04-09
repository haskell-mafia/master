{-# LANGUAGE TemplateHaskell #-}
module Master.TH (
    gitVersion
  ) where

import           Control.Applicative
import           Data.Version
import           System.Process
import           Paths_master
import           Language.Haskell.TH

[d| gitVersion :: String
    gitVersion = $(runIO $ do
        let cbv = showVersion version
        ver  <- readProcess "git" ["log", "--pretty=format:%h", "-n", "1"] ""
        time <- init <$> readProcess "date" ["+%Y%m%d%H%M%S"] ""
        notModified <- ((>) 1 . length) `fmap` readProcess "git" ["status", "--porcelain"] ""
        let str = cbv ++ "-" ++ time ++ "-" ++ ver
        return $ (LitE . StringL) $ if notModified
            then str
            else str ++ "-M")|]
