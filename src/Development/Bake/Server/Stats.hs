{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns, CPP #-}

module Development.Bake.Server.Stats(
    stats,
    record, recordIO
    ) where

import Control.DeepSeq
import Control.Applicative
import Development.Bake.Core.Type
import Development.Bake.Server.Type
import Data.IORef
import Data.Monoid
import Data.List.Extra
import General.HTML
import General.Extra
import GHC.Stats
import System.IO.Unsafe
import System.Time.Extra
import Control.Exception
import Data.Tuple.Extra
import Numeric.Extra
import qualified Data.Map as Map


data Stat = Stat {statHistory :: [Double], statCount :: !Int, statSum :: !Double, statMax :: !Double}

instance Monoid Stat where
    mempty = Stat [] 0 0 0
    mappend (Stat x1 x2 x3 x4) (Stat y1 y2 y3 y4) = Stat (take 10 $ x1 ++ y1) (x2+y2) (x3+y3) (x4 `max` y4)


{-# NOINLINE recorded #-}
recorded :: IORef (Map.Map String Stat)
recorded = unsafePerformIO $ newIORef Map.empty

record :: NFData b => String -> (a -> b) -> a -> b
record msg f x = unsafePerformIO $ recordIO msg $ return $ f x

recordIO :: NFData a => String -> IO a -> IO a
recordIO msg x = do
    (d, x) <- duration $ do x <- x; evaluate $ rnf x; return x
    atomicModifyIORef recorded $ (,()) .  Map.insertWith mappend msg (Stat [d] 1 d d)
    return x

stats :: Server -> IO HTML
stats Server{..} = do
    recorded <- readIORef recorded
#if __GLASGOW_HASKELL__ < 706
    getGCStatsEnabled <- return True
#else
    getGCStatsEnabled <- getGCStatsEnabled
#endif
    stats <- if getGCStatsEnabled then Just <$> getGCStats else return Nothing
    rel <- relativeTimestamp
    return $ do
        p_ $ str_ $ "Requests = " ++ show (length history) ++ ", updates = " ++ show (length updates)

        h2_ $ str_ "Sampled statistics"
        let ms x = show $ (ceiling $ x * 1000 :: Integer)
        table ["Counter","Count","Mean (ms)","Max (ms)","Last 10 (ms)"]
            [ map str_ [name, show statCount, ms $ statSum / intToDouble statCount
                       ,ms statMax, unwords $ map ms statHistory] 
            | (name,Stat{..}) <- Map.toList recorded]

        h2_ $ str_ "Requests per client"
        table ["Client","Requests","Utilisation (last hour)","Utilisation"]
            [ map str_ [fromClient c, show $ length xs, f $ 60*60, f $ maximum $ map fst3 xs]
            | c <- Map.keys pings
              -- how long ago you started, duration
            , let xs = [(rel t, maybe (rel t) aDuration a, qThreads q) | (t,q,a) <- history, qClient q == c]
            , not $ null xs
            , let f z = show (floor $ sum [ max 0 $ intToDouble threads * (dur - max 0 (start - z))
                                          | (start,dur,threads) <- xs] * 100 / z) ++ "%"]

            -- how many seconds ago you started, duration
            -- start dur z   max 0 (start-z)
            -- 75 10 60 = 0   15
            -- 65 10 60 = 5   5
            -- 55 10 60 = 10  0

        h2_ $ str_ "GHC statistics"
        case stats of
            Nothing -> p_ $ str_ "No GHC stats, rerun with +RTS -T"
            Just x -> pre_ $ str_ $ replace ", " "\n" $ takeWhile (/= '}') $ drop 1 $ dropWhile (/= '{') $ show x


table :: [String] -> [[HTML]] -> HTML
table cols body = table_ $ do
    thead_ $ tr_ $ mconcat $ map (td_ . str_) cols
    tbody_ $ mconcat $ [tr_ $ mconcat $ map td_ x | x <- body]
