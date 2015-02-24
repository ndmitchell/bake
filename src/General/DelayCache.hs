{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

-- | Add a way to ask information about patches.
module General.DelayCache(
    DelayCache, newDelayCache, addDelayCache, askDelayCache
    ) where

import General.Extra
import Control.Concurrent.Extra
import Control.Monad
import qualified Data.Map as Map


data DelayCache k v = DelayCache
    {lock :: Lock -- thing to hold while computing a patch (to throttle)
    ,info :: Var (Map.Map k v)
    }

instance Show (DelayCache k v) where
    show _ = "DelayCache"

newDelayCache :: IO (DelayCache k v)
newDelayCache = do
    lock <- newLock
    var <- newVar Map.empty
    return $ DelayCache lock var


-- | If you add multiple things only the first will be computed
addDelayCache :: Ord k => DelayCache k v -> k -> IO v -> IO ()
addDelayCache DelayCache{..} k v =
    void $ forkSlave $
        withLock lock $ do
            i <- readVar info
            unless (k `Map.member` i) $ do
                v <- v
                modifyVar_ info $ return . Map.insert k v

askDelayCache :: Ord k => DelayCache k v -> IO (k -> Maybe v)
askDelayCache DelayCache{..} = do
    i <- readVar info
    return $ \k -> Map.lookup k i
