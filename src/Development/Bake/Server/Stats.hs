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


stats :: Prettys -> Memory -> (Maybe Test -> HTML) -> IO HTML
stats Prettys{..} Memory{..} showTest = do
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

    now <- getCurrentTime
    let periods = [addSeconds (negate x*24*60*60) now | x <- [1,7,30,365]]
    let one [Only x] = x
        one _ = error "Didn't get one"
    plausibleCount :: [Int] <- forM periods $ \p -> one <$> storeSQL store "SELECT count(*) FROM patch WHERE plausible IS NOT NULL AND queue > ?" (Only p)
    plausibleAvg :: [Double] <- forM periods $ \p -> one <$> storeSQL store "SELECT ifnull(avg(julianday(plausible)-julianday(queue)),0) FROM patch WHERE plausible IS NOT NULL AND queue > ?" (Only p)
    percentiles <- forM [100,95,90,80,75,50,25,10,0] $ \perc -> (perc,) <$> do
        forM (zip periods plausibleCount) $ \(p,count) -> do
            let n = min (count - 1) $ ((count * perc) `div` 100)
            one <$> storeSQL store "SELECT julianday(plausible)-julianday(queue) AS x FROM patch WHERE plausible IS NOT NULL AND queue > ? ORDER BY x ASC LIMIT ?, 1" (p,n)

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
            in f (i_ $ str_ "All") slowestAll : [f (showTest test) x | (Only test :. x) <- slowest]

        h2_ $ str_ "Most common rejection tests (max 10)"
        table ["Test","Rejections"] [[showTest t, str_ $ show x] | (t, x) <- rejections]

        h2_ $ str_ "Speed to plausible"
        table ["Plausible","Last day","Last week","Last month","Last year"] $
            let f x = str_ $ showDuration $ x*24*60*60
                perc 100 = "Maximum"
                perc 0 = "Minimum"
                perc x = show x ++ "% within" in
            (str_ "Count" : map (str_ . show) plausibleCount) :
            (str_ "Average" : map f plausibleAvg) :
            [str_ (perc p) : map f xs | (p,xs) <- percentiles]

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
