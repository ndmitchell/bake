{-# LANGUAGE RankNTypes, TupleSections, GADTs, RecordWildCards #-}

module Development.Bake.Server.Memory(
    ClientInfo(..), Memory(..),
    newMemory, stateFailure,
    Shower(..), shower,
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
import General.HTML
import Control.Monad
import General.Extra


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
    -- READER
    {simulated :: Bool
        -- ^ Are we running in a simulation (don't spawn separate process)
    ,oven :: Oven State Patch Test
        -- ^ The oven under test
    ,prettys :: Prettys
        -- ^ The pretty functions

    -- STATE
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
    }

newMemory :: Oven State Patch Test -> Prettys -> Store -> (State, Answer) -> IO Memory
newMemory oven prettys store (state, answer) = do
    store <- storeUpdate store [IUState state answer Nothing]
    let ps = map fst $ sortOn (paQueued . snd) $
             filter (isJust . paStart . snd) $
             map (id &&& storePatch store) $ Set.toList $ storeAlive store
    return $ Memory False oven prettys [] store [] Map.empty [] False (state, ps)

instance NFData Memory where
    rnf Memory{..} = ()


data Shower = Shower
    {showLink :: String -> HTML -> HTML
    ,showPatch :: Patch -> HTML
    ,showExtra :: Either State Patch -> HTML
    ,showTest :: Maybe Test -> HTML
    ,showTestAt :: (State, [Patch]) -> Maybe Test -> HTML
    ,showQuestion :: Question -> HTML
    ,showClient :: Client -> HTML
    ,showState :: State -> HTML
    ,showCandidate :: (State, [Patch]) -> HTML
    ,showTime :: UTCTime -> HTML
    ,showThreads :: Int -> HTML
    }

shower :: Memory -> Bool -> IO Shower
shower Memory{prettys=Prettys{..},..} argsAdmin = do
    showRel <- showRelativeTime
    let shwState s | s == toState "" = span__ [class_ "bad" ] $ str_ $ "invalid state"
        shwState s = shwLink ("state=" ++ fromState s) $ str_ $ prettyState s
    let shwPatch p = shwLink ("patch=" ++ fromPatch p) $ str_ $ prettyPatch p
    return $ Shower
        {showLink = shwLink
        ,showPatch = shwPatch
        ,showState = shwState
        ,showCandidate = \(s,ps) -> do
            shwState s
            when (not $ null ps) $ str_ " plus " <> commas_ (map shwPatch ps)
        ,showExtra = \e -> raw_ $ maybe "" fst $ storeExtra store e
        ,showClient = \c -> shwLink ("client=" ++ url_ (fromClient c)) $ str_ $ fromClient c
        ,showTest = f Nothing Nothing []
        ,showTestAt = \(s,ps) -> f Nothing (Just s) ps
        ,showQuestion = \Question{..} -> f (Just qClient) (Just $ fst qCandidate) (snd qCandidate) qTest
        ,showTime = \x -> span__ [class_ "nobr"] $ str_ $ showUTCTime "%H:%M" x ++ " (" ++ showRel x ++ ")"
        ,showThreads = \i -> str_ $ show i ++ " thread" ++ ['s' | i /= 1]
        }
    where
        shwLink url = a__ [href_ $ (if argsAdmin then "?admin=&" else "?") ++ url]

        f c s ps t =
            shwLink (intercalate "&" parts) $ str_ $
            maybe "Preparing" prettyTest t
            where parts = ["client=" ++ url_ (fromClient c) | Just c <- [c]] ++
                          ["state=" ++ url_ (fromState s) | Just s <- [s]] ++
                          ["patch=" ++ url_ (fromPatch p) | p <- ps] ++
                          ["test=" ++ url_ (maybe "" fromTest t)]
