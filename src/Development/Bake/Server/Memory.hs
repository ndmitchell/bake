{-# LANGUAGE RankNTypes, TupleSections, GADTs, RecordWildCards #-}

module Development.Bake.Server.Memory(
    ClientInfo(..), Memory(..),
    newMemory, stateFailure
    ) where

import Data.IORef
import Development.Bake.Server.Store
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Bake.Core.Type
import Data.Time
import Control.Monad.Extra
import Data.Maybe
import Data.Tuple.Extra
import Development.Bake.Core.Message
import System.IO.Unsafe
import General.Extra

stateFailure = State ""


---------------------------------------------------------------------
-- THE DATA TYPE

data ClientInfo = ClientInfo
    {ciPingTime :: UTCTime
    ,ciPing :: Ping
    ,ciAlive :: Bool
    ,ciDone :: Set.Set ((State, [Patch]), Maybe Test) -- all were successful
    } deriving (Eq,Show)

data Memory = Memory
    {simulated :: Bool
        -- ^ Are we running in a simulation (don't spawn separate process)
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
    ,active :: (State, [Patch])
        -- ^ the target we are working at (some may already be rejected)
    ,skipped :: Map.Map Test String
        -- ^ tests which are currently skipped
    }

newMemory :: Store -> State -> Memory
newMemory store state = Memory False store [] Map.empty [] False (state, []) Map.empty
