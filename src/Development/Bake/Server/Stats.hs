{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns, ScopedTypeVariables, TypeOperators #-}

module Development.Bake.Server.Stats(
    stats,
    record, recordIO
    ) where

import Control.DeepSeq
import Control.Applicative
import Control.Monad
import Development.Bake.Core.Type
import Development.Bake.Server.Brain
import Database.SQLite.Simple
import Data.IORef
import Data.Monoid
import Data.List.Extra
import General.HTML
import General.Extra
import Development.Bake.Server.Store
import GHC.Stats
import System.IO.Unsafe
import System.Time.Extra
import Control.Exception
import Numeric.Extra
import qualified Data.Map as Map
import Prelude


data Stat = Stat {statHistory :: [Double], statCount :: !Int, statSum :: !Double, statMax :: !Double}

instance Monoid Stat where
    mempty = Stat [] 0 0 0
    mappend (Stat x1 x2 x3 x4) (Stat y1 y2 y3 y4) = Stat (take 10 $ x1 ++ y1) (x2+y2) (x3+y3) (x4 `max` y4)


{-# NOINLINE recorded #-}
recorded :: IORef (Map.Map String Stat)
recorded = unsafePerformIO $ newIORef Map.empty

record :: NFData b => (a -> ([String], b)) -> a -> b
record f x = unsafePerformIO $ recordIO $ return $ f x

recordIO :: NFData a => IO ([String], a) -> IO a
recordIO x = do
    (d, (msg,x)) <- duration $ do x <- x; evaluate $ rnf x; return x
    forM_ (inits msg) $ \msg ->
        atomicModifyIORef recorded $ (,()) .  Map.insertWith mappend (unwords msg) (Stat [d] 1 d d)
    return x


stats :: Prettys -> Memory -> IO HTML
stats Prettys{..} Memory{..} = do
    recorded <- readIORef recorded
    getGCStatsEnabled <- getGCStatsEnabled
    stats <- if getGCStatsEnabled then Just <$> getGCStats else return Nothing
    rel <- relativeTime

    [Only (patchCount :: Int)] <- storeSQL store "SELECT count(*) FROM patch" ()
    [Only (stateCount :: Int)] <- storeSQL store "SELECT count(*) FROM state" ()
    [Only (runCount :: Int)] <- storeSQL store "SELECT count(*) FROM run" ()

    slowest :: [Only (Maybe Test) :. (Int, Seconds, Seconds, Seconds)] <- storeSQL store "SELECT test, count(*), avg(duration) as avg, sum(duration), max(duration) FROM run GROUP BY test ORDER BY avg DESC LIMIT 25" ()
    [slowestAll :: (Int, Seconds, Seconds, Seconds)] <- storeSQL store "SELECT count(*), avg(duration) as avg, sum(duration), max(duration) FROM run" ()
    rejections :: [(Maybe Test, Int)] <- storeSQL store "SELECT test, count(*) AS n FROM reject WHERE test IS NULL OR test NOT IN (SELECT test FROM skip) GROUP BY test ORDER BY n DESC LIMIT 10" ()

    [Only (plausibleCount :: Int)] <- storeSQL store "SELECT count(*) FROM patch WHERE plausible IS NOT NULL" ()
    [Only (plausibleAvgAll :: Double)] <- storeSQL store "SELECT ifnull(avg(julianday(plausible)-julianday(queue)),0) FROM patch WHERE plausible IS NOT NULL" ()
    [Only (plausibleAvgWeek :: Double)] <- storeSQL store "SELECT ifnull(avg(julianday(plausible)-julianday(queue)),0) FROM patch WHERE plausible IS NOT NULL AND queue > datetime(julianday('now')-7)" ()
    percentiles <- if plausibleCount == 0 then return [] else forM [100,95,90,80,75,50,25,10,0] $ \p -> do
        let q = Only $ min (plausibleCount - 1) $ ((plausibleCount * p) `div` 100)
        [Only (all :: Double)] <- storeSQL store "SELECT julianday(plausible)-julianday(queue) AS x FROM patch WHERE plausible IS NOT NULL ORDER BY x ASC LIMIT ?, 1" q
        [Only (week :: Double)] <- storeSQL store "SELECT julianday(plausible)-julianday(queue) AS x FROM patch WHERE plausible IS NOT NULL AND queue > datetime(julianday('now')-7) ORDER BY x ASC LIMIT ?, 1" q
        return (p, week, all)

    return $ do
        p_ $ str_ $ "Patches = " ++ show patchCount ++ ", states = " ++ show stateCount ++ ", runs = " ++ show runCount

        h2_ $ str_ "Sampled statistics"
        let ms x = show $ (ceiling $ x * 1000 :: Integer)
        table ["Counter","Count","Mean (ms)","Sum (ms)","Max (ms)","Last 10 (ms)"]
            [ (if null name then i_ $ str_ "All" else str_ name) :
              map str_ [show statCount, ms $ statSum / intToDouble statCount, ms statSum
                       ,ms statMax, unwords $ map ms statHistory] 
            | (name,Stat{..}) <- Map.toAscList recorded]

        h2_ $ str_ "Slowest tests (max 25)"
        table ["Test","Count","Mean","Sum","Max"] $
            let f name (count, avg, sum, max) = name : map str_ [show count, showDuration avg, showDuration sum, showDuration max]
            in f (i_ $ str_ "All") slowestAll : [f (str_ $ maybe "Preparing" fromTest test) x | (Only test :. x) <- slowest]

        h2_ $ str_ "Most common rejection tests (max 10)"
        table ["Test","Rejections"] [[str_ $ maybe "Preparing" fromTest t, str_ $ show x] | (t, x) <- rejections]

        h2_ $ str_ "Speed to plausible"
        table ["Plausible","This week","Forever"] $
            let f x = str_ $ showDuration $ x*24*60*60 in
            [str_ "Average", f plausibleAvgWeek, f plausibleAvgAll] :
            [[str_ $ "Percentile " ++ show p, f a, f b] | (p,a,b) <- percentiles]

        h2_ $ str_ "GHC statistics"
        case stats of
            Nothing -> p_ $ str_ "No GHC stats, rerun with +RTS -T"
            Just x@GCStats{..} -> do
                ul_ $ do
                    li_ $ str_ $ "Uptime of: " ++ showDuration wallSeconds
                    li_ $ str_ $ "Haskell memory usage: " ++ show peakMegabytesAllocated ++ "Mb"
                    li_ $ str_ $ "CPU time: " ++ showDuration cpuSeconds ++
                          " (mutator: " ++ showDuration mutatorCpuSeconds ++ ", GC: " ++ showDuration gcCpuSeconds ++ ")"
                pre_ $ str_ $ replace ", " "\n" $ takeWhile (/= '}') $ drop 1 $ dropWhile (/= '{') $ show x


table :: [String] -> [[HTML]] -> HTML
table cols body = table_ $ do
    thead_ $ tr_ $ mconcat $ map (td_ . str_) cols
    tbody_ $ mconcat $ [tr_ $ mconcat $ map td_ x | x <- body]
