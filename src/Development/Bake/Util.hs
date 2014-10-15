{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Development.Bake.Util(
    Timestamp(..), getTimestamp
    ) where

import Data.Time.Clock
import System.IO.Unsafe
import Data.IORef
import Data.Tuple.Extra


data Timestamp = Timestamp UTCTime Int deriving (Show,Eq)

{-# NOINLINE timestamp #-}
timestamp :: IORef Int
timestamp = unsafePerformIO $ newIORef 0

getTimestamp :: IO Timestamp
getTimestamp = do
    t <- getCurrentTime
    i <- atomicModifyIORef timestamp $ dupe . (+1)
    return $ Timestamp t i
