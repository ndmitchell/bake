{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, TupleSections, CPP #-}

module General.Extra(
    UTCTime, getCurrentTime, addSeconds, showRelativeTime, relativeTime, showUTCTime, readDate,
    createDir,
    withFileLock,
    pick,
    memoIO0, memoIO1,
    catMaybesSet,
    whenLeft, whenRight,
    timeInit, timed, time, time_,
    eitherToMaybe,
    newCVar, readCVar, modifyCVar, modifyCVar_,
    registerMaster, forkSlave,
    Worker, newWorker,
    makeRelativeEx,
    transitiveClosure, findCycle,
    putBlock,
    maybe',
    commas, commasLimit, unwordsLimit
    ) where

import Data.Time.Clock
import Data.Time.Calendar
import System.Time.Extra
import System.IO.Unsafe
import System.IO.Extra
import Data.IORef
import Data.List.Extra
import System.Directory.Extra
import Data.Hashable
import System.FilePath
import Control.Exception.Extra
import Control.Applicative
import Control.Monad.Extra
import Control.Concurrent.Extra
import Development.Shake.Command
import Data.Maybe
import System.Random
import Data.Either.Extra
import Data.Time.Format
#if __GLASGOW_HASKELL__< 710
import System.Locale
#endif
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Prelude


addSeconds :: Seconds -> UTCTime -> UTCTime
addSeconds x = addUTCTime (fromRational $ toRational x)

relativeTime :: IO (UTCTime -> Seconds)
relativeTime = do
    now <- getCurrentTime
    return $ \old -> subtractTime now old

showRelativeTime :: IO (UTCTime -> String)
showRelativeTime = do
    now <- getCurrentTime
    return $ \old ->
        let secs = subtractTime now old
            mins = secs / 60
            hours = mins / 60
            days = toModifiedJulianDay (utctDay now) - toModifiedJulianDay (utctDay old) in
        if days > 3 then show days ++ " days ago"
        else if hours > 5 then show (round hours) ++ " hours ago"
        else if mins > 2 then show (round mins) ++ " mins ago"
        else show (max 2 $ round secs) ++ " secs ago"

showUTCTime :: String -> UTCTime -> String
showUTCTime = formatTime defaultTimeLocale

readDate :: String -> UTCTime
readDate s = fromMaybe (error $ "Invalid date, expected something like 2012-10-28, got " ++ s) $
    parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) "2012-12-13" :: UTCTime


{-# NOINLINE logTime #-}
logTime :: IO Seconds
logTime = unsafePerformIO offsetTime

timeInit :: IO ()
timeInit = void logTime


{-# NOINLINE createDirLock #-}
createDirLock :: Lock
createDirLock = unsafePerformIO newLock

createDir :: String -> [String] -> IO FilePath
createDir prefix info = do
    let name = prefix ++ (if null info then "" else "-" ++ show (abs $ hash info))
    createDirectoryIfMissing True name
    withLock createDirLock $ writeFile (name </> ".bake.name") $ unlines info
    return name


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)


timed :: String -> IO a -> IO a
timed msg act = do
    (tim,res) <- duration act
    tot <- logTime
    putStrLn $ "[BAKE-TIME] " ++ showDuration tim ++ " (total of " ++ showDuration tot ++ "): " ++ msg
    return res

time_ :: IO (CmdLine, CmdTime) -> IO ()
time_ act = time $ do (a,b) <- act; return (a,b,())

time :: IO (CmdLine, CmdTime, a) -> IO a
time act = do
    (CmdLine msg, CmdTime tim, res) <- act
    tot <- logTime
    putStrLn $ "[BAKE-TIME] " ++ showDuration tim ++ " (total of " ++ showDuration tot ++ "): " ++ msg
    return res

makeRelativeEx :: FilePath -> FilePath -> IO FilePath
makeRelativeEx x y = do
    x <- splitDirectories <$> canonicalizePath x
    y <- splitDirectories <$> canonicalizePath y
    return $ joinPath $ if take 1 x /= take 1 y then y else f x y
    where
        f (x:xs) (y:ys)
            | x == y = f xs ys
            | otherwise = ".." : f xs (y:ys)
        f _ ys = ys


-- Might be better off using the 'filelock' package
withFileLock :: FilePath -> IO a -> IO a
withFileLock lock act = do
    -- important to canonicalize everything as the act might change the current directory
    createDirectoryIfMissing True $ takeDirectory lock
    lock <- (</> takeFileName lock) <$> canonicalizePath (takeDirectory lock)

    let stamp = lock <.> "stamp"
    let touch = do t <- show <$> getCurrentTime; ignore $ writeFile stamp t; return t
    unlessM (doesFileExist stamp) $ void touch

    (t,_) <- duration $ whileM $ do
        b <- try_ $ createDirectory lock
        if isRight b then do
            return False
         else do
            sleep 10
            now <- getCurrentTime
            mtime <- try_ $ getModificationTime stamp
            case mtime of
                Right x | addSeconds 30 x > now -> return True
                _ -> do
                    -- try and take ownership of the stamp
                    me <- touch
                    sleep 10 -- wait for the stamp to settle down
                    src <- try_ $ readFile' stamp
                    return $ either (const True) (/= me) src
    putStrLn $ "Waited " ++ showDuration t ++ " to acquire the file lock " ++ lock

    active <- newVar True
    touch
    thread <- forkSlave $ forever $ do
        sleep 10
        withVar active $ \b -> when b $ void touch
    act `finally` do
        modifyVar_ active $ const $ return False
        killThread thread
        ignore $ removeDirectory lock

{-
tester :: IO ()
tester = do
    forM_ [1..2] $ \i -> do
        forkIO $ withFileLock "mylock" $ do
                print ("start", i)
                sleep 60
                print ("stop",i)
    sleep 1000
-}

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

forkSlave :: IO () -> IO ThreadId
forkSlave act = forkFinally act $ \v -> case v of
    Left e | fromException e /= Just ThreadKilled -> do
        m <- readIORef master
        whenJust m $ flip throwTo e
    _ -> return ()

type Worker = IO () -> IO ()

newWorker :: IO Worker
newWorker = do
    lock <- newLock
    return $ \act -> void $ forkIO $ withLock lock act


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


memoIO0 :: IO b -> IO (IO b)
memoIO0 act = return $ unsafeInterleaveIO act

memoIO1 :: (Hashable a, Eq a) => (a -> IO b) -> IO (a -> IO b)
memoIO1 op = do
    var <- newVar HashMap.empty
    return $ \k -> modifyVar var $ \mp ->
        case HashMap.lookup k mp of
            Just v -> return (mp, v)
            Nothing -> do
                v <- op k
                return (HashMap.insert k v mp, v)


catMaybesSet :: Ord a => Set.Set (Maybe a) -> Set.Set a
catMaybesSet = Set.mapMonotonic fromJust . Set.delete Nothing

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft x f = either f (const $ pure ()) x

whenRight :: Applicative m => Either a b -> (b -> m ()) -> m ()
whenRight x f = either (const $ pure ()) f x


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

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' x nothing just = maybe nothing just x
