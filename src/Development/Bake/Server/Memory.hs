{-# LANGUAGE RankNTypes, TupleSections, GADTs, RecordWildCards #-}

module Development.Bake.Server.Memory(
    ClientInfo(..), Memory(..),
    newMemory, stateFailure
    ) where

import Development.Bake.Server.Store
import qualified Data.Map as Map
import Development.Bake.Core.Type
import Data.Time
import Development.Bake.Core.Message
import Control.DeepSeq
import qualified Data.Set as Set
import Data.Tuple.Extra
import Data.List.Extra
import Data.Maybe

stateFailure = toState ""


---------------------------------------------------------------------
-- THE DATA TYPE

data ClientInfo = ClientInfo
    {ciPingTime :: UTCTime
    ,ciPing :: Ping
    ,ciAlive :: Bool
    ,ciTests :: Map.Map (Point, Maybe Test) Bool -- if a single failure, set to False
    } deriving (Eq,Show)

data Memory = Memory
    {simulated :: Bool
        -- ^ Are we running in a simulation (don't spawn separate process)
    ,admins :: [Author]
        -- ^ People responsible for overall administration
    ,store :: Store
        -- ^ All the information on disk
    ,fatal :: [String]
        -- ^ A list of fatal error messages that have been raised by the server
    ,clients :: Map.Map Client ClientInfo
        -- ^ Latest time of a ping sent by each client
    ,running :: [(UTCTime, Question)]
        -- ^ Questions you have sent to clients and are waiting for.
    ,paused :: Bool
        -- ^ Pretend the queued is empty
    ,active :: Point
        -- ^ The target we are working at (some may already be rejected).
        --   Note that when restarting, we throw away the rejected ones.
    } deriving Show

newMemory :: Store -> (State, Answer) -> IO Memory
newMemory store (state, answer) = do
    store <- storeUpdate store [IUState state answer Nothing]
    let ps = map fst $ sortOn (paQueued . snd) $
             filter (isJust . paStart . snd) $
             map (id &&& storePatch store) $ Set.toList $ storeAlive store
    return $ Memory False [] store [] Map.empty [] False (state, ps)

instance NFData Memory where
    rnf Memory{..} = ()
