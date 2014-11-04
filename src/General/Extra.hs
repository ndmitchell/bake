{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module General.Extra(
    Timestamp(..), getTimestamp, showRelativeTimestamp,
    createDir
    ) where

import Data.Time.Clock
import Data.Time.Calendar
import System.Time.Extra
import System.IO.Unsafe
import Data.IORef
import Data.Tuple.Extra
import System.Directory
import Data.Hashable
import System.FilePath


data Timestamp = Timestamp UTCTime Int deriving (Show,Eq)

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

