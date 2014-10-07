{-# LANGUAGE ScopedTypeVariables #-}

module Development.Bake.Util(
    sleep, duration,
    withTempFile, withTempDir,
    withCurrentDirectory, withTempDirCurrent,
    (&&^), whenJust,
    showException,
    fst3, snd3, thd3,
    unit,
    try_, handle_,
    strip,
    rep, reps,
    partitionM
    ) where

import qualified System.IO.Temp as T
import System.IO
import Extra


withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile template act = T.withSystemTempFile template $ \file h -> hClose h >> act file

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = T.withSystemTempDirectory "bake"

withTempDirCurrent :: IO a -> IO a
withTempDirCurrent act = withTempDir $ \t -> withCurrentDirectory t act
