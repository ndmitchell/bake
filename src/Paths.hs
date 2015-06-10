-- | Fake cabal module for local building
{-# OPTIONS_GHC -w #-}

module Paths_bake where

import Data.Version
import System.IO.Unsafe
import System.Directory
import Control.Exception


getDataDir :: IO FilePath
getDataDir = unsafePerformIO $ do
    -- so that it gets cached the first time
    x <- getCurrentDirectory
    return $ do
        evaluate $ length x
        return x

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
