{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Development.Bake.Util(
    Timestamp(..), getTimestamp,
    createDir
    ) where

import Data.Time.Clock
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

createDir :: String -> [String] -> IO FilePath
createDir prefix info = do
    let name = prefix ++ "-" ++ show (abs $ hash info)
    writeFile (name <.> "txt") $ unlines info
    createDirectoryIfMissing True name
    return name

