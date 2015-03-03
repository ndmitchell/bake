{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, TupleSections, CPP #-}

module General.Extra(
    UTCTime, getCurrentTime, addSeconds, showRelativeTime, relativeTime, showUTCTime,
    createDir,
    withFileLock,
    pick,
    timed,
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
import System.IO.Extra
import Data.IORef
import Data.List.Extra
import System.Directory.Extra
import Data.Hashable
import System.FilePath
import Control.Exception.Extra
import Control.Monad.Extra
import Control.Concurrent.Extra
import System.Random
import Data.Either.Extra
import Data.Time.Format
#if __GLASGOW_HASKELL__< 710
import System.Locale
#endif
import qualified Data.Set as Set


addSeconds :: Double -> UTCTime -> UTCTime
addSeconds x = addUTCTime (fromRational $ toRational x)

relativeTime :: IO (UTCTime -> Double)
relativeTime = do
    now <- getCurrentTime
    return $ \old -> subtractTime now old

showRelativeTime :: IO (UTCTime -> String)
showRelativeTime = do
    now <- getCurrentTime
    return $ \old ->
        let secs = subtractTime now old
            days = toModifiedJulianDay . utctDay
            poss = [(days now - days old, "day")
                   ,(floor $ secs / (60*60), "hour")
                   ,(floor $ secs / 60, "min")
                   ,(max 1 $ floor secs, "sec")
                   ]
            (i,s) = head $ dropWhile ((==) 0 . fst) poss
        in show i ++ " " ++ s ++ ['s' | i /= 1] ++ " ago"

showUTCTime :: String -> UTCTime -> String
showUTCTime = formatTime defaultTimeLocale


{-# NOINLINE createDirLock #-}
createDirLock :: Lock
createDirLock = unsafePerformIO newLock

createDir :: String -> [String] -> IO FilePath
createDir prefix info = do
    let name = prefix ++ (if null info then "" else "-" ++ show (abs $ hash info))
    createDirectoryIfMissing True name
    withLock createDirLock $ writeFile (name </> "bake.name") $ unlines info
    return name


pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)


timed :: String -> IO a -> IO a
timed msg act = do
    (t,r) <- duration act
    putStrLn $ "Spent " ++ showDuration t ++ " on " ++ msg
    return r


withTempTemplate :: FilePath -> (FilePath -> IO a) -> IO a
withTempTemplate file act = bracket
    (uncurry openTempFile $ splitFileName file)
    (\(file, hndl) -> do hClose hndl; ignore $ removeFile file)
    (\(file, hndl) -> do hClose hndl; act file)


withFileLock :: FilePath -> IO a -> IO a
withFileLock file act = do
    createDirectoryIfMissing True $ takeDirectory file
    active <- newVar True
    dir <- getCurrentDirectory
    withTempTemplate file $ \tmp -> do
        hPutStrLn stderr $ show (dir, file, tmp)
        whileM $ do
            hPutStrLn stderr "writing out temp file"
            writeFile tmp ""
            hPutStrLn stderr "done writing out temp file"
            mtime <- try_ $ getModificationTime file
            hPutStrLn stderr $ show mtime
            now <- getCurrentTime
            case mtime of
                Right x | addSeconds 60 x < now -> sleep 10 >> return True
                _ -> do
                    hPutStrLn stderr "trying to rename"
                    b <- try_ $ renameFile tmp file
                    hPutStrLn stderr $ show b
                    if isRight b then return False else sleep 10 >> return True

    -- important to canonicalize as the act might change the current directory
    file <- canonicalizePath file
    thread <- forkSlave $ forever $ do
        sleep 30
        withVar active $ \b -> when b $ do
            hPutStrLn stderr "tickling temp file"
            writeFile file ""
            hPutStrLn stderr "done tickling temp file"
    act `finally` do
        modifyVar_ active $ const $ return False
        killThread thread
        ignore $ removeFile file


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
