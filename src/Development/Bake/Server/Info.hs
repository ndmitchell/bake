{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Info(
    Info, newInfo,
    infoTodo, infoRunning, infoSuccess, infoFailure,
    infoBlessed, infoRejected,
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import Development.Bake.Server.Type
import General.Extra
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import General.Lens
import Data.Monoid
import Data.List
import Data.Maybe


type List1 a = [a]

newtype Point = Point Int deriving (Eq,Ord)

data PointInfo = PointInfo
    {piRunning :: Map.Map (Maybe Test) (List1 (Timestamp, Question))
    ,piSuccess :: Map.Map (Maybe Test) (List1 (Timestamp, Question, Answer))
    ,piFailure :: Map.Map (Maybe Test) (List1 (Timestamp, Question, Answer))
    }

data Info = Info
    {iPoints :: Map.Map (State, [Patch]) Point
    ,iPointInfo :: Map.Map Point PointInfo
    ,iUpdates :: Map.Map State (State, [Patch])
    }


instance Monoid PointInfo where
    mempty = PointInfo mempty mempty mempty
    mappend (PointInfo x1 x2 x3) (PointInfo y1 y2 y3) = PointInfo (x1<>y1) (x2<>y2) (x3<>y3)

_piRunning = makeLens piRunning $ \x v -> v{piRunning=x}
_piSuccess = makeLens piSuccess $ \x v -> v{piSuccess=x}
_piFailure = makeLens piFailure $ \x v -> v{piFailure=x}

instance Monoid Info where
    mempty = Info mempty mempty mempty
    mappend (Info x1 x2 x3) (Info y1 y2 y3) = Info (x1<>y1) (x2<>y2) (x3<>y3)

_iPoints = makeLens iPoints $ \x v -> v{iPoints=x}
_iPointInfo = makeLens iPointInfo $ \x v -> v{iPointInfo=x}


newInfo :: Server -> Info
newInfo s = foldl' addHistory mempty{iUpdates=states} $ history s
    where
        states = Map.fromList [(new, old) | (_, new, Just old) <- updates s]

        addHistory :: Info -> (Timestamp, Question, Maybe Answer) -> Info
        addHistory i (t, q, a) = i2 & _iPointInfo . atm p %~ add
            where (i2,p) = addPoint (qCandidate q) i
                  add pi | Just a <- a = pi & (if aSuccess a then _piSuccess else _piFailure) . atm (qTest q)
                                         %~ (:) (t,q,a)
                         | otherwise = pi & _piRunning . atm (qTest q) %~ (:) (t,q)

        addPoint :: (State, [Patch]) -> Info -> (Info, Point)
        addPoint (s, ps) i | Just (s', ps') <- Map.lookup s states =
                             let (i', p) = addPoint (s', ps' ++ ps) i
                             in (i' & _iPoints . at (s, ps) .~ Just p, p)
        addPoint (s, ps) i = (i & _iPoints . at (s, ps) .~ Just p, p)
            where p = Point $ Map.size (iPoints i)


infoTodo :: Info -> (State, [Patch]) -> Maybe (Set.Set Test)
infoTodo Info{..} sp = do
    p <- Map.lookup sp iPoints
    PointInfo{..} <- Map.lookup p iPointInfo
    (_,_,a):_ <- Map.lookup Nothing piSuccess
    return $ Set.fromList $ aTests a

infoRunning :: Info -> (State, [Patch]) -> Set.Set (Maybe Test)
infoRunning Info{..} sp = fromMaybe Set.empty $ do
    p <- Map.lookup sp iPoints
    PointInfo{..} <- Map.lookup p iPointInfo
    return $ Map.keysSet piRunning

infoSuccess :: Info -> (State, [Patch]) -> Set.Set (Maybe Test)
infoSuccess Info{..} sp = fromMaybe Set.empty $ do
    p <- Map.lookup sp iPoints
    PointInfo{..} <- Map.lookup p iPointInfo
    return $ Map.keysSet piSuccess

infoFailure :: Info -> (State, [Patch]) -> Set.Set (Maybe Test)
infoFailure Info{..} sp = fromMaybe Set.empty $ do
    p <- Map.lookup sp iPoints
    PointInfo{..} <- Map.lookup p iPointInfo
    return $ Map.keysSet piFailure

infoBlessed :: Info -> (State, [Patch]) -> Bool
infoBlessed i sp | Set.null $ infoFailure i sp, Just t <- infoTodo i sp = Set.map Just t `Set.isSubsetOf` infoSuccess i sp
                 | otherwise = False

infoRejected :: Info -> (State, [Patch]) -> Set.Set (Maybe Test)
infoRejected i (s,[]) | Just sp <- Map.lookup s $ iUpdates i = infoRejected i sp
                      | otherwise = infoFailure i (s,[])
infoRejected i (s,ps) = Set.filter f $ infoFailure i (s,ps)
    where f t = t `Set.member` infoSuccess i (s, init ps)
