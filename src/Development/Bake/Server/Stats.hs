{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Server.Stats(
    stats,
    record, recordIO
    ) where

import Control.DeepSeq
import Development.Bake.Server.Type


record :: NFData b => String -> (a -> b) -> a -> b
record _ f x = f x

recordIO :: NFData a => String -> IO a -> IO a
recordIO _ x = x

stats :: Server -> IO String
stats _ = return "statistics here"

