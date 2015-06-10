-- | Fake cabal module for local building
{-# OPTIONS_GHC -w #-}

module Paths_bake where

import Data.Version
import System.IO.Unsafe
import System.Directory
import Control.Exception
import System.Environment.Extra


getDataDir :: IO FilePath
getDataDir = unsafePerformIO $ do
    -- so that it gets cached the first time
    x <- maybe getCurrentDirectory return =<< lookupEnv "bake_datadir"
    return $ do
        evaluate $ length x
        return x

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
