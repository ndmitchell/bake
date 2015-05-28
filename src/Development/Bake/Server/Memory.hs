{-# LANGUAGE RankNTypes, TupleSections, GADTs, RecordWildCards #-}

module Development.Bake.Server.Memory(
    PingEx(..), PointEx(..), Memory(..), Cache(..),
    newMemory, newCache, stateFailure
    ) where

import Data.IORef
import Development.Bake.Server.Disk
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Bake.Core.Type
import Data.Time
import Control.Monad.Extra
import Data.Maybe
import Data.Tuple.Extra
import Development.Bake.Core.Message
import System.IO.Unsafe
import General.Extra

stateFailure = State ""


---------------------------------------------------------------------
-- THE DATA TYPE

data PingEx = PingEx
    {piTime :: UTCTime
    ,piPing :: Ping
    ,piAlive :: Bool
    } deriving (Eq,Show)

data Memory = Memory
    -- STATIC
    {simulated :: Bool
        -- ^ Are we running in a simulation (don't spawn separate process)
    ,disk :: Disk
        -- ^ All the information on disk

    -- MANIPULATED BY BRAIN
    ,fatal :: [String]
        -- ^ A list of fatal error messages that have been raised by the server
    ,pings :: Map.Map Client PingEx
        -- ^ Latest time of a ping sent by each client
    ,running :: [(UTCTime, Question)]
        -- ^ Questions you have sent to clients and are waiting for.
    ,paused :: Bool
        -- ^ Pretend the queued is empty
    ,active :: (State, [Patch])
        -- ^ the target we are working at (some may already be rejected)
    }

newMemory :: Disk -> State -> Memory
newMemory disk state = Memory False disk [] Map.empty [] False (state, [])


data Cache = Cache
    {patches :: Map.Map Patch UTCTime
        -- ^ Patches, the time they were submitted
    ,alive :: Set.Set Patch
        -- ^ Patches which are of interest because they need dealing with
    ,skipped :: Map.Map Test String
        -- ^ Tests that are currently marked to be skipped, so are not run
    ,points :: Map.Map Point (State, [Patch])
        -- ^ Points and their real data
    ,queued :: [Patch]
        -- ^ Who is queued up, namely alive and not in active
    ,supersetPass :: Set.Set Test
        -- ^ Tests that have been passed by a superset of Patch
    ,selfInfo :: PointEx
        -- ^ Information about active
    ,previousInfo :: [PointEx]
        -- ^ Information about each point before active, will be length (drop 1 $ snd active)
    }


{-# NOINLINE newCacheDisk #-}
newCacheDisk :: Disk -> IO CacheDisk
newCacheDisk = unsafePerformIO $ memo1 $ \disk -> do
    ps <- computePatches disk
    let patches = fst <$> ps
    let alive = snd <$> ps
    skipped <- computeSkipped disk
    points <- computePoints disk
    return CacheDisk{..}    


{-# NOINLINE newCacheActive #-}
newCacheActive :: (Disk, (State, [Patch])) -> IO CacheActive
newCacheActive = unsafePerformIO $ memo1 $ \(disk,active) -> do
    undefined


newCache :: Memory -> IO Cache
newCache Memory{..} = do
    CacheDisk{..} <- newCacheDisk disk
    CacheActive{..} <- newCacheActive (disk, active)
    patches <- patches_
    return Cache{..}

-- Depends only on Disk
data CacheDisk = CacheDisk
    -- DEPENDS ONLY ON DISK
    {patches_ :: IO (Map.Map Patch UTCTime)
        -- ^ Patches, the time they were submitted
    ,alive_ :: IO (Set.Set Patch)
        -- ^ Patches which are of interest because they need dealing with
    ,skipped_ :: IO (Map.Map Test String)
        -- ^ Tests that are currently marked to be skipped, so are not run
    ,points_ :: IO (Map.Map Point (State, [Patch]))
        -- ^ Points and their real data
    }

-- Depends on Disk and Active
data CacheActive = CacheActive
    {queued_ :: IO [Patch]
        -- ^ Who is queued up
    ,supersetPass_ :: IO (Set.Set Test)
        -- ^ Tests that have been passed by a superset of Patch
    ,selfInfo_ :: IO PointEx
        -- ^ Information about this item
    ,previousInfo_ :: IO [PointEx]
        -- ^ Information about each point before that
    }


---------------------------------------------------------------------
-- CACHES

compute :: (Disk -> IO a) -> (forall result . Q result -> result -> a -> a) -> Disk -> IO (IO a)
compute op add disk = do
    -- Warning: not race condition safe, if disk changes while I am observing it
    v <- newIORef =<< op disk
    undo <- notify disk $ \a b -> writeIORef v . add a b =<< readIORef v
    mkWeakIORef v undo
    return $ readIORef v


computeSkipped :: Disk -> IO (IO (Map.Map Test String))
computeSkipped = compute initial increment
    where
        initial disk = do
            ss <- load disk SkipList
            fmap (Map.fromList . catMaybes) $ forM ss $ \s -> fmap (s,) <$> load disk (SkipAuthor s)

        increment :: Q result -> result -> Map.Map Test String -> Map.Map Test String
        increment (SkipAuthor s) Nothing = Map.delete s
        increment (SkipAuthor s) (Just a) = Map.insert s a
        increment _ _ = id


computePatches :: Disk -> IO (IO (Map.Map Patch UTCTime, Set.Set Patch)) -- (queue time, active set)
computePatches = compute initial increment
    where
        initial disk = do
            ps <- load disk PatchList
            xs <- forM ps $ \p -> do
                start <- load disk $ PatchSubmitted p
                active <- andM [null <$> load disk (PatchRejected p)
                               ,isNothing <$> load disk (PatchDeleted p)
                               ,isNothing <$> load disk (PatchMerged p)
                               ,isNothing <$> load disk (PatchPlausible p)
                               ,isNothing <$> load disk (PatchSuperseded p)]
                return (p, (start, active))
            return (Map.fromList $ map (second fst) xs, Set.fromList $ map fst $ filter (snd . snd) xs)

        increment :: Q result -> result -> (Map.Map Patch UTCTime, Set.Set Patch) -> (Map.Map Patch UTCTime, Set.Set Patch)
        increment (PatchSubmitted p) t = first (Map.insert p t) . second (Set.insert p)
        increment (PatchRejected p) v = inactive p $ not $ null v
        increment (PatchDeleted p) v = inactive p $ isJust v
        increment (PatchMerged p) v = inactive p $ isJust v
        increment (PatchPlausible p) v = inactive p $ isJust v
        increment (PatchSuperseded p) v = inactive p $ isJust v
        increment _ _ = id

        inactive p True = second (Set.delete p)
        inactive _ _ = error "Internal violation, patch becoming active"


computePoints :: Disk -> IO (IO (Map.Map Point (State, [Patch])))
computePoints = compute initial increment
    where
        initial disk = do
            x <- load disk PointList
            fmap Map.fromList $ forM x $ \i -> (i,) <$> load disk (PointResolve i)

        increment :: Q result -> result -> Map.Map Point (State, [Patch]) -> Map.Map Point (State, [Patch])
        increment (PointResolve i) r = Map.insert i r
        increment _ _ = id


data PointEx = PointEx
    {pointTests :: Maybe (Set.Set Test)
        -- ^ The list of required tests (if available)
    ,pointRuns :: Map.Map (Maybe Test) [(Bool, Client)]
        -- ^ For each test the pass/fail for each client
    ,pointRejected :: Set.Set (Maybe Test)
        -- ^ Tests for which this node was rejected
    ,pointPlausible :: Bool
        -- ^ Is the point now plausible
    }

{-
cacheWorking :: Disk -> IO (Map.Map Point (State, [Patch])) -> (State, [Patch]) -> IO (IO Working)
cacheWorking disk points (s,ps) = cache create increment disk
    where
        create disk = return $ Working Set.empty (replicate (length ps - 1) z) z Set.empty Set.empty
            where z = (Nothing, Map.empty)

        increment :: Q result -> result -> Working -> Working
        increment _ _ = id
-}
