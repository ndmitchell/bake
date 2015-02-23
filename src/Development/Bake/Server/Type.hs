{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Point, newPoint,
    PointInfo(..), PatchInfo(..),
    PingInfo(..), addPing, sFailure,
    UpdateInfo(..), addUpdate, ensurePauseInvariants,
    Server(..), server0, state0,
    Question(..), Answer(..), Ping(..),
    serverConsistent, serverPrune,
    addAnswer, addQuestion,
    deletePatch, clearPatches, addPatch, rejectPatch,
    startPause, stopPause
    ) where

import Control.Applicative
import Data.Monoid
import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Extra
import General.Equal
import General.Str
import General.Lens
import General.DelayCache
import Data.Tuple.Extra
import Data.Maybe
import Control.Monad
import Data.List.Extra
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set


---------------------------------------------------------------------
-- THE DATA TYPE

newtype Point = Point (Equal [Patch]) deriving (Eq,Ord,Show)

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

data PointInfo = PointInfo
    {poTests :: Maybe (Set Test)
    ,poPass :: Map (Maybe Test) [(UTCTime, Question, Answer)]
    ,poFail :: Map (Maybe Test) [(UTCTime, Question, Answer)]
    ,poReject :: Map (Maybe Test) [(UTCTime, Question, Answer)]
    } deriving Show

instance Monoid PointInfo where
    mempty = PointInfo mempty mempty mempty mempty
    mappend (PointInfo x1 x2 x3 x4) (PointInfo y1 y2 y3 y4) =
        PointInfo (x1<>y1) (x2<>y2) (x3<>y3) (x4<>y4)

data PatchInfo = PatchInfo
    {paReject :: Map (Maybe Test) [(UTCTime, Question, Answer)]
    ,paPass :: Set (Maybe Test)
    }

instance Monoid PatchInfo where
    mempty = PatchInfo mempty mempty
    mappend (PatchInfo x1 x2) (PatchInfo y1 y2) = PatchInfo (x1<>y1) (x2<>y2)

data Server = Server
    {history :: [(UTCTime, Question, Maybe Answer)]
        -- ^ Questions you have sent to clients, and how they responded (if they have).
    ,pointInfo :: Map Point PointInfo
        -- ^ Information about a point
    ,patchInfo :: Map Patch PatchInfo
        -- ^ Information about a patch
    ,rejectable :: Map (Point, Maybe Test) [(Point, (UTCTime, Question, Answer))]
        -- ^ Things whose success lead to a rejection
    ,updates :: [UpdateInfo]
    ,updatesIdx :: Map State [Patch]
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
server0 = Server [] Map.empty Map.empty Map.empty [] Map.empty Map.empty (error "server0: target") Nothing [] Map.empty (error "server0: extra") []

state0 :: Server -> State
state0 Server{..} = uiState $ last updates

addAnswer :: Question -> Answer -> Server -> Server
addAnswer q@Question{..} a@Answer{..} server
    | (pre,(t,_,_):post) <- break ((==) q . snd3) $ history server =
        let pt = newPoint server qCandidate
        in
        -- add to poTodo
        (\s -> let todo = Set.fromList $ aTests a in
               case s ^. _pointInfo . atm pt . _poTests of
                   _ | qTest /= Nothing -> s
                   Just v | v /= todo -> s & _fatal %~ (:) ("Inconsistent tests for " ++ show qCandidate)
                   _ -> s & _pointInfo . atm pt . _poTests .~ Just todo) $
        -- add to poPass/poFail
        (_pointInfo . atm pt . (if aSuccess then _poPass else _poFail) . atm qTest %~ (:) (t, q, a)) $
        -- add to paPass
        (if not aSuccess || null (snd qCandidate) then id
         else _patchInfo . atm (last $ snd qCandidate) . _paPass %~ Set.insert qTest) $
        -- update rejectables
        (\s -> case rewindPoint pt of
            Just (prev, patch)
                | not aSuccess, _:_ <- s ^. _pointInfo . atm prev . _poPass . atm qTest
                    -> s & (_pointInfo . atm pt . _poReject . atm qTest %~ (:) (t, q, a)) .
                           (_patchInfo . atm patch . _paReject . atm qTest %~ (:) (t, q, a))
                | not aSuccess -> s & _rejectable . atm (prev, qTest) %~ (:) (pt, (t, q, a))
            _ | aSuccess, xs <- s ^. _rejectable . atm (pt, qTest)
                -> foldr ($) (s & _rejectable . at (pt, qTest) .~ Nothing)
                        [   (_pointInfo . atm x . _poReject . atm qTest %~ (:) tqa)
                          . (_patchInfo . atm (snd $ fromJust $ rewindPoint x) . _paReject . atm qTest %~ (:) tqa)
                        | (x,tqa) <- xs]
            _ -> s) $
        -- add to history
        server{history = pre ++ (t,q,Just a) : post}
    | otherwise = server

newPoint :: Server -> (State, [Patch]) -> Point
newPoint Server{..} (s, ps) = Point $ newEqual $ (fromMaybe err $ Map.lookup s updatesIdx) ++ ps
    where err = error $ "newPoint, failed to resolve state " ++ show s ++ ", not in " ++ show (Map.keys updatesIdx)

rewindPoint :: Point -> Maybe (Point, Patch)
rewindPoint (Point pt) = first (Point . newEqual) <$> unsnoc (fromEqual pt)


addQuestion :: UTCTime -> Question -> Server -> Server
addQuestion now q server = server{history = (now,q,Nothing) : history server}

addUpdate :: UTCTime -> Answer -> Maybe State -> (State, [Patch]) -> Server -> Server
addUpdate now answer (Just snew) (sold,ps) server@Server{..} | aSuccess answer = ensurePauseInvariants
    server{target=(snew, snd target \\ ps)
          ,updates=UpdateInfo now answer snew (Just (sold,ps)):updates
          ,updatesIdx=Map.insert snew ((updatesIdx Map.! sold) ++ ps) updatesIdx}
addUpdate now answer _ (sold,ps) server@Server{..} =
    server{fatal = "Failed to update" : fatal
          ,updates=UpdateInfo now answer sFailure (Just (sold,ps)):updates
          ,updatesIdx=Map.insert sFailure [] updatesIdx}

ensurePauseInvariants :: Server -> Server
ensurePauseInvariants server
    | null $ snd $ target server = server
        {paused = Nothing
        ,target = second (++ fromMaybe [] (paused server)) $ target server}
    | otherwise = server

clearPatches :: Server -> Server
clearPatches server = server{paused = Nothing, target = (fst $ target server, [])}

deletePatch :: Patch -> Server -> Server
deletePatch p server = ensurePauseInvariants $ server
    {target = second (delete p) $ target server
    ,paused = delete p <$> paused server
    }

addPatch :: UTCTime -> Author -> Patch -> Server -> Server
addPatch now author p server
    | p `elem` concatMap (maybe [] snd . uiPrevious) (updates server)
        -- gets confusing if a patch is both included AND active
        = server
    | p `elem` snd (target server)
        -- moving a promotion requires retrying every previous promotion
        = server
    | otherwise = server
        {target = second (if isJust (paused server) then id else add) $ target server
        ,paused = add <$> paused server
        ,authors = Map.insertWith (++) (Just p) [author] $ authors server
        ,submitted = (now,p) : submitted server}
        where add ps = filter (/= p) ps `snoc` p

rejectPatch :: Patch -> Server -> Server
rejectPatch p server = server{target=second (delete p) $ target server}

startPause :: Server -> Server
startPause server = ensurePauseInvariants $ server{paused = Just $ fromMaybe [] $ paused server}

stopPause :: Server -> Server
stopPause server = ensurePauseInvariants $ server{paused = Just $ fromMaybe [] $ paused server}


---------------------------------------------------------------------
-- CHECKS ON THE SERVER

-- any question that has been asked of a client who hasn't pinged since the time is thrown away
serverPrune :: UTCTime -> Server -> Server
serverPrune cutoff s
    | null died = s
    | otherwise = s{history = filter (flip notElem died . qClient . snd3) $ history s
                   ,pings = Map.map (\pi@PingInfo{..} -> pi{piAlive = piAlive && pClient piPing `notElem` died}) $ pings s}
    where died = [pClient piPing | PingInfo{..} <- Map.elems $ pings s, piTime < cutoff, piAlive]

addPing :: UTCTime -> Ping -> Server -> Server
addPing now ping s = s{pings = Map.insert (pClient ping) (PingInfo now ping True) $ pings s}

serverConsistent :: Server -> IO ()
serverConsistent Server{..} = do
    let xs = groupSort $ map (qCandidate . snd3 &&& id) $ filter (isNothing . qTest . snd3) history
    forM_ xs $ \(c,vs) -> do
        case nubOrd $ map (sort . aTests) $ filter aSuccess $ mapMaybe thd3 vs of
            a:b:_ -> error $ "serverConsistent: Tests don't match for candidate: " ++ show (c,a,b,vs)
            _ -> return ()


---------------------------------------------------------------------
-- LENSES

_pointInfo = makeLens pointInfo $ \v x -> x{pointInfo=v}
_patchInfo = makeLens patchInfo $ \v x -> x{patchInfo=v}
_poTests = makeLens poTests $ \v x -> x{poTests=v}
_poPass = makeLens poPass $ \v x -> x{poPass=v}
_poFail = makeLens poFail $ \v x -> x{poFail=v}
_poReject = makeLens poReject $ \v x -> x{poReject=v}
_paReject = makeLens paReject $ \v x -> x{paReject=v}
_paPass = makeLens paPass $ \v x -> x{paPass=v}
_fatal = makeLens fatal $ \v x -> x{fatal=v}
_rejectable = makeLens rejectable $ \v x -> x{rejectable=v}
