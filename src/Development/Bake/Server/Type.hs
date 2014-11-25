{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..),
    Question(..), Answer(..), Ping(..),
    consistent, prune,
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Extra
import General.DelayCache
import Data.Time.Clock
import Data.Tuple.Extra
import Data.Maybe
import Control.Monad
import Data.List.Extra


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
    ,extra :: DelayCache Patch (String, String)
        -- ^ Extra information that was computed for each string (cached forever)
    }


-- any question that has been asked of a client who hasn't pinged since the time is thrown away
prune :: UTCTime -> Server -> Server
prune cutoff s = s{history = filter (flip elem clients . qClient . snd3) $ history s}
    where clients = [pClient | (Timestamp t _,Ping{..}) <- pings s, t >= cutoff]


consistent :: Server -> IO ()
consistent Server{..} = do
    let xs = groupSort $ map (qCandidate . snd3 &&& id) $ filter (isNothing . qTest . snd3) history
    forM_ xs $ \(c,vs) -> do
        case nub $ map (sort . uncurry (++) . aTests) $ filter aSuccess $ mapMaybe thd3 vs of
            a:b:_ -> error $ "Tests don't match for candidate: " ++ show (c,a,b,vs)
            _ -> return ()
