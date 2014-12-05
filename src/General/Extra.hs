{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, TupleSections #-}

module General.Extra(
    Timestamp(..), getTimestamp, showRelativeTimestamp,
    createDir,
    newCVar, readCVar, modifyCVar, modifyCVar_,
    registerMaster, forkSlave,
    transitiveClosure, findCycle,
    putBlock,
    commas, commasLimit, unwordsLimit
    ) where

import Data.Time.Clock
import Data.Time.Calendar
import System.Time.Extra
import System.IO.Unsafe
import Data.IORef
import Data.Tuple.Extra
import Data.List.Extra
import System.Directory
import Data.Hashable
import System.FilePath
import Control.Monad.Extra
import Control.Concurrent.Extra
import qualified Data.Set as Set


data Timestamp = Timestamp UTCTime Int deriving Show

instance Eq Timestamp where Timestamp _ a == Timestamp _ b = a == b
instance Ord Timestamp where compare (Timestamp _ a) (Timestamp _ b) = compare a b

{-# NOINLINE timestamp #-}
timestamp :: IORef Int
timestamp = unsafePerformIO $ newIORef 0

getTimestamp :: IO Timestamp
getTimestamp = do
    t <- getCurrentTime
    i <- atomicModifyIORef timestamp $ dupe . (+1)
    return $ Timestamp t i

showRelativeTimestamp :: IO (Timestamp -> String)
showRelativeTimestamp = do
    now <- getCurrentTime
    return $ \(Timestamp old _) ->
        let secs = subtractTime now old
            days = toModifiedJulianDay . utctDay
            poss = [(days now - days old, "day")
                   ,(floor $ secs / (60*60), "hour")
                   ,(floor $ secs / 60, "min")
                   ,(max 1 $ floor secs, "sec")
                   ]
            (i,s) = head $ dropWhile ((==) 0 . fst) poss
        in show i ++ " " ++ s ++ ['s' | i /= 1] ++ " ago"

createDir :: String -> [String] -> IO FilePath
createDir prefix info = do
    let name = prefix ++ "-" ++ show (abs $ hash info)
    writeFile (name <.> "txt") $ unlines info
    createDirectoryIfMissing True name
    return name


---------------------------------------------------------------------
-- CVAR

-- | A Var, but where readCVar returns the last cached value
data CVar a = CVar {cvarCache :: Var a, cvarReal :: Var a}

newCVar :: a -> IO (CVar a)
newCVar x = liftM2 CVar (newVar x) (newVar x)

readCVar :: CVar a -> IO a
readCVar = readVar . cvarCache

modifyCVar :: CVar a -> (a -> IO (a, b)) -> IO b
modifyCVar CVar{..} f =
    modifyVar cvarReal $ \a -> do
        (a,b) <- f a
        modifyVar_ cvarCache $ const $ return a
        return (a,b)

modifyCVar_ :: CVar a -> (a -> IO a) -> IO ()
modifyCVar_ cvar f = modifyCVar cvar $ fmap (,()) . f


---------------------------------------------------------------------
-- SLAVE/MASTER

{-# NOINLINE master #-}
master :: IORef (Maybe ThreadId)
master = unsafePerformIO $ newIORef Nothing

registerMaster :: IO ()
registerMaster = writeIORef master . Just =<< myThreadId

forkSlave :: IO () -> IO ()
forkSlave act = void $ forkFinally act $ \v -> case v of
    Left e -> do
        m <- readIORef master
        whenJust m $ flip throwTo e
    _ -> return ()


---------------------------------------------------------------------
-- UTILITIES

-- | Given a relation and a starting value, find the transitive closure.
--   The resulting list will be a set.
transitiveClosure :: Ord a => (a -> [a]) -> [a] -> [a]
transitiveClosure follow xs = f Set.empty xs
    where
        f seen [] = []
        f seen (t:odo) | t `Set.member` seen = f seen odo
                       | otherwise = t : f (Set.insert t seen) (follow t ++ odo)


-- | Given a relation and a starting list, find a cycle if there is one.
--   The resulting list will be a set, and will contain a cycle (but not necessarily be minimal).
findCycle :: Ord a => (a -> [a]) -> [a] -> Maybe [a]
findCycle follow = firstJust $ \x ->
    let children = transitiveClosure follow (follow x)
    -- if there is a cycle, make the element we know is cyclic first, so its easier to debug
    in if x `elem` children then Just (x : delete x children) else Nothing


---------------------------------------------------------------------
-- FORMATTING

putBlock :: String -> [String] -> IO ()
putBlock title body = putStrLn $ unlines $
    let s = "-- " ++ title ++ " --" in
    (s ++ replicate (70 - length s) '-') :
    body ++
    [replicate 70 '-']

commas :: [String] -> String
commas = intercalate ", "

commasLimit :: Int -> [String] -> String
commasLimit = limit commas

unwordsLimit :: Int -> [String] -> String
unwordsLimit = limit unwords

limit :: ([String] -> String) -> Int -> [String] -> String
limit rejoin i xs = rejoin a ++ (if null b then "" else "...")
    where (a,b) = splitAt i xs
