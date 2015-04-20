-- | Fake cabal module for local building
{-# OPTIONS_GHC -w #-}

module Paths_bake where

import Data.Version

getDataDir :: IO FilePath
getDataDir = return "."

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
