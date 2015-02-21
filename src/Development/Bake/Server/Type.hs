{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    PingInfo(..), addPing, sFailure,
    UpdateInfo(..), addUpdate, ensurePauseInvariants,
    Server(..), server0, state0,
    Question(..), Answer(..), Ping(..),
    serverConsistent, serverPrune,
    normalise, translate,
    addAnswer, addQuestion
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Extra
import General.Str
import General.DelayCache
import Data.Tuple.Extra
import Data.Maybe
import Control.Monad
import Data.List.Extra
import Data.Map(Map)
import qualified Data.Map as Map


---------------------------------------------------------------------
-- THE DATA TYPE

data PingInfo = PingInfo
    {piTime :: UTCTime
    ,piPing :: Ping
    ,piAlive :: Bool
    } deriving (Eq,Show)

data UpdateInfo = UpdateInfo
    {uiTime :: UTCTime
    ,uiAnswer :: Answer
    ,uiState :: State
    ,uiPrevious :: Maybe (State, [Patch])
    } deriving (Eq,Show)

data Server = Server
    {history :: [(UTCTime, Question, Maybe Answer)]
        -- ^ Questions you have sent to clients, and how they responded (if they have).
    ,updates :: [UpdateInfo]
        -- ^ Updates that have been made. If the Answer failed, you must have an entry in fatal
    ,pings :: Map Client PingInfo
        -- ^ Latest time of a ping sent by each client
    ,target :: (State, [Patch])
        -- ^ The candidate we are currently aiming to prove
    ,paused :: Maybe [Patch]
        -- ^ 'Just' if we are paused, and the number of people queued up (reset when target becomes Nothing)
    ,submitted :: [(UTCTime, Patch)]
        -- ^ List of all patches that have been submitted over time
    ,authors :: Map (Maybe Patch) [Author]
        -- ^ Authors associated with each patch (Nothing is the server author)
    ,extra :: DelayCache (Either State Patch) (Str, Str)
        -- ^ Extra information that was computed for each string (cached forever)
    ,fatal :: [String]
        -- ^ A list of fatal error messages that have been raised by the server
    }

sFailure :: State
sFailure = State ""


-- | Warning: target and extra are undefined, either define them or don't ever use them
server0 :: Server
server0 = Server [] [] Map.empty (error "server0: target") Nothing [] Map.empty (error "server0: extra") []

state0 :: Server -> State
state0 Server{..} = uiState $ last updates

addAnswer :: Question -> Answer -> Server -> Server
addAnswer qq aa server
    | (pre,(t,_,_):post) <- break ((==) qq . snd3) $ history server = server{history = pre ++ (t,qq,Just aa) : post}
    | otherwise = server

addQuestion :: UTCTime -> Question -> Server -> Server
addQuestion now q server = server{history = (now,q,Nothing) : history server}

addUpdate :: UTCTime -> Answer -> Maybe State -> (State, [Patch]) -> Server -> Server
addUpdate now answer (Just snew) (sold,ps) server | aSuccess answer = ensurePauseInvariants
    server{target=(snew, snd (target server) \\ ps), updates=UpdateInfo now answer snew (Just (sold,ps)):updates server}
addUpdate now answer _ (sold,ps) server =
    server{fatal = "Failed to update" : fatal server, updates=UpdateInfo now answer sFailure (Just (sold,ps)):updates server}

ensurePauseInvariants :: Server -> Server
ensurePauseInvariants server
    | null $ snd $ target server = server
        {paused = Nothing
        ,target = second (++ fromMaybe [] (paused server)) $ target server}
    | otherwise = server


---------------------------------------------------------------------
-- CHECKS ON THE SERVER

-- any question that has been asked of a client who hasn't pinged since the time is thrown away
serverPrune :: UTCTime -> Server -> Server
serverPrune cutoff s = s{history = filter (flip elem clients . qClient . snd3) $ history s}
    where clients = [pClient piPing | PingInfo{..} <- Map.elems $ pings s, piTime >= cutoff]

addPing :: UTCTime -> Ping -> Server -> Server
addPing now ping s = s{pings = Map.insert (pClient ping) (PingInfo now ping True) $ pings s}

serverConsistent :: Server -> IO ()
serverConsistent Server{..} = do
    let xs = groupSort $ map (qCandidate . snd3 &&& id) $ filter (isNothing . qTest . snd3) history
    forM_ xs $ \(c,vs) -> do
        case nub $ map (sort . aTests) $ filter aSuccess $ mapMaybe thd3 vs of
            a:b:_ -> error $ "serverConsistent: Tests don't match for candidate: " ++ show (c,a,b,vs)
            _ -> return ()


---------------------------------------------------------------------
-- STATE/PATCH ISOMORPISMS

normalise :: Server -> (State, [Patch]) -> (State, [Patch]) -> (State, [Patch], [Patch])
normalise = f . updates
    where
        f _ (s1,p1) (s2,p2) | s1 == s2 = (s1,p1,p2)
        f (UpdateInfo{uiState=s, uiPrevious=Just (s',ps)}:us) s1 s2 = f us (g s1) (g s2)
            where g (s1,p1) = if s1 == s then (s',ps++p1) else (s1,p1)
        f _ s1 s2 = error $ "Error with normalise, invariant about state violated: " ++ show (s1, s2)

translate :: Server -> State -> (State, [Patch]) -> Maybe [Patch]
translate server s1 (s2,p2) = stripPrefix pp1 pp2
    where (_,pp1,pp2) = normalise server (s1,[]) (s2,p2)
