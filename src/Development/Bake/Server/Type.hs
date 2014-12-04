{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..), server0, state0,
    Question(..), Answer(..), Ping(..),
    serverConsistent, serverPrune,
    normalise, translate
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Extra
import General.Str
import General.DelayCache
import Data.Time.Clock
import Data.Tuple.Extra
import Data.Maybe
import Control.Monad
import Data.List.Extra
import Data.Map(Map)
import qualified Data.Map as Map


---------------------------------------------------------------------
-- THE DATA TYPE

data Server = Server
    {history :: [(Timestamp, Question, Maybe Answer)]
        -- ^ Questions you have sent to clients, and how they responded (if they have).
    ,updates :: [(Timestamp, State, Maybe (State, [Patch]))]
        -- ^ Updates that have been made
    ,pings :: Map Client (Timestamp, Ping)
        -- ^ Latest time of a ping sent by each client
    ,target :: (State, [Patch])
        -- ^ The candidate we are currently aiming to prove
    ,paused :: Maybe [(Timestamp, Patch)]
        -- ^ 'Just' if we are paused, and the number of people queued up (reset when target becomes Nothing)
    ,submitted :: [(Timestamp, Patch)]
        -- ^ List of all patches that have been submitted over time
    ,authors :: Map (Maybe Patch) [Author]
        -- ^ Authors associated with each patch (Nothing is the server author)
    ,extra :: DelayCache (Either State Patch) (Str, Str)
        -- ^ Extra information that was computed for each string (cached forever)
    ,logs :: [(Timestamp, Maybe (State, [Patch]), Answer)]
        -- ^ History of init and updates
    ,fatal :: [String]
        -- ^ A list of fatal error messages that have been raised by the server
    }


-- | Warning: target and extra are undefined, either define them or don't ever use them
server0 :: Server
server0 = Server [] [] Map.empty (error "server0: target") Nothing [] Map.empty (error "server0: extra") [] []

state0 :: Server -> State
state0 Server{..} = snd3 $ last updates


---------------------------------------------------------------------
-- CHECKS ON THE SERVER

-- any question that has been asked of a client who hasn't pinged since the time is thrown away
serverPrune :: UTCTime -> Server -> Server
serverPrune cutoff s = s{history = filter (flip elem clients . qClient . snd3) $ history s}
    where clients = [pClient | (Timestamp t _,Ping{..}) <- Map.elems $ pings s, t >= cutoff]


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
        f ((_,s,Just (s',ps)):us) s1 s2 = f us (g s1) (g s2)
            where g (s1,p1) = if s1 == s then (s',ps++p1) else (s1,p1)
        f _ s1 s2 = error $ "Error with normalise, invariant about state violated: " ++ show (s1, s2)

translate :: Server -> State -> (State, [Patch]) -> Maybe [Patch]
translate server s1 (s2,p2) = stripPrefix pp1 pp2
    where (_,pp1,pp2) = normalise server (s1,[]) (s2,p2)
