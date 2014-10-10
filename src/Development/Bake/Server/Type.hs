
-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..), defaultServer,
    Question(..), Answer(..), Ping(..),
    Timestamp(..), getTimestamp
    ) where

import Development.Bake.Type
import Development.Bake.Message
import Data.Time.Clock
import System.IO.Unsafe
import Data.IORef
import Data.Tuple.Extra


defaultServer :: State -> Server
defaultServer s = Server [] [] [] (s,[]) Nothing [] [] []

data Server = Server
    {history :: [(Timestamp, Question, Maybe Answer)]
        -- ^ Questions you have sent to clients, and how they responded (if they have).
        --   The aStdout has been written to disk, and the value is a filename containing the stdout.
    ,updates :: [(Timestamp, State, (State, [Patch]))]
        -- ^ Updates that have been made
    ,pings :: [(Timestamp, Ping)]
        -- ^ Latest time of a ping sent by each client
    ,active :: (State, [Patch])
        -- ^ The candidate we are currently aiming to prove
    ,paused :: Maybe [(Timestamp, Patch)]
        -- ^ 'Just' if we are paused, and the number of people queued up
    ,submitted :: [(Timestamp, Patch)]
        -- ^ List of all patches that have been submitted over time
    ,authors :: [(Maybe Patch, Author)]
        -- ^ Authors associated with each patch (Nothing is the server author)
    ,extra :: [(Patch, (String, String))]
        -- ^ Extra information that was computed for each string (cached forever)
    } deriving Show


data Timestamp = Timestamp UTCTime Int deriving (Show,Eq)

{-# NOINLINE timestamp #-}
timestamp :: IORef Int
timestamp = unsafePerformIO $ newIORef 0

getTimestamp :: IO Timestamp
getTimestamp = do
    t <- getCurrentTime
    i <- atomicModifyIORef timestamp $ dupe . (+1)
    return $ Timestamp t i
