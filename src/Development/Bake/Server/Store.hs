{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}

-- Stuff on disk on the server
module Development.Bake.Server.Store(
    Store, newStore,
    PointInfo(..), poTest, PatchInfo(..), StateInfo(..),
    storePatches, storePatch, storeState, storeStates, storeAlive, storePoint, storeSupersetPass,
    Update(..), storeUpdate,
    storeSaveTest, storeLoadTest,
    storeSaveUpdate, storeLoadUpdate
    ) where

import Development.Bake.Server.Database
import Development.Bake.Core.Type
import Development.Bake.Core.Message
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import Data.Monoid
import General.Extra
import Data.Maybe
import Control.Monad
import System.Directory
import Database.SQLite.Simple
import System.FilePath


data PointInfo = PointInfo
    {poTodo :: Maybe (Set.Set Test)
    ,poPass :: Set.Set (Maybe Test)
    ,poFail :: Set.Set (Maybe Test) -- may be in both pass and fail
    } deriving Show

instance Monoid PointInfo where
    mempty = PointInfo Nothing Set.empty Set.empty
    mappend (PointInfo x1 x2 x3) (PointInfo y1 y2 y3) = PointInfo (x1 <> y1) (x2 <> y2) (x3 <> y3)

poTest :: PointInfo -> Maybe Test -> Maybe Bool
poTest PointInfo{..} t
    | t `Set.member` poFail = Just False
    | t `Set.member` poPass = Just True
    | Just t <- t, Just todo <- poTodo, not $ t `Set.member` todo = Just True -- It's not applicable, and thus passes
    | otherwise = Nothing


data PatchInfo = PatchInfo
    {paQueued :: (UTCTime, Author)
    ,paStart :: Maybe UTCTime
    ,paDelete :: Maybe UTCTime
    ,paSupersede :: Maybe UTCTime
    ,paReject :: Maybe (UTCTime, Map.Map (Maybe Test) (State, [Patch]))
    ,paPlausible :: Maybe UTCTime
    ,paMerge :: Maybe UTCTime
    }
    deriving Show

data StateInfo = StateInfo
    {stCreated :: UTCTime
    ,stSource :: Maybe (State, [Patch])
    }

data Store = Store
    {conn :: Connection
    ,path :: FilePath
    ,time :: UTCTime
    ,stateMap :: Map.Map State StateInfo -- ^ Time I found out about this state, Point it was created from
    ,patchInfo :: Map.Map Patch PatchInfo -- ^ Information about all patches
    ,patchAlive :: Set.Set Patch -- ^ Patches that are alive
    ,points :: Map.Map (State, [Patch]) PointInfo -- needs to be in IO for cache properties
    -- ,supersets :: Maybe (Point, Set.Set Test) -- needs to be in IO for cache properties
    }

newStore :: FilePath -> IO Store
newStore path = do
    time <- getCurrentTime
    createDirectoryIfMissing True path
    conn <- create (path </> "bake.sqlite")
    return $ Store conn path time Map.empty Map.empty Set.empty Map.empty

storePoint :: Store -> (State,[Patch]) -> PointInfo
storePoint Store{..} x = Map.findWithDefault mempty x points

storePatches :: Store -> Map.Map Patch PatchInfo
storePatches = patchInfo

storePatch :: Store -> Patch -> PatchInfo
storePatch store p = storePatches store Map.! p

storeState :: Store -> State -> StateInfo
storeState store st = storeStates store Map.! st

storeStates :: Store -> Map.Map State StateInfo
storeStates = stateMap

storeAlive :: Store -> Set.Set Patch
storeAlive Store{..} = Map.keysSet $ Map.filter isAlive patchInfo
    where isAlive PatchInfo{..} = isNothing paDelete && isNothing paReject && isNothing paMerge && isNothing paSupersede


storeSupersetPass :: Store -> (State,[Patch]) -> Set.Set Test
storeSupersetPass Store{..} (s,ps) = Set.unions $ map passed $ Map.elems $ Map.filterWithKey isSuperset points
    where
        isSuperset (s2, ps2) _ = s == s2 && f ps ps2
        passed PointInfo{..} = catMaybesSet $ poPass `Set.difference` poFail

        f (x:xs) (y:ys)
            | x == y = f xs ys
            | otherwise = f (x:xs) ys
        f [] _ = True
        f _ [] = False



data Update
    = IUState State (Maybe (State, [Patch]))
    | IUQueue Patch Author
    | IUStart Patch
    | IUDelete Patch
    | IUReject Patch (Maybe Test) (State, [Patch])
    | IUPlausible Patch
    | IUSupersede Patch
    | IUMerge Patch
    | PUTest (State, [Patch]) [Test]
    | PUPass (State, [Patch]) (Maybe Test)
    | PUFail (State, [Patch]) (Maybe Test)
      deriving Show


ensurePoint :: Store -> (State, [Patch]) -> IO Point
ensurePoint Store{..} (s, ps) = do
    let v = (fromState s, concatMap (\x -> "[" ++ fromPatch x ++ "]") ps)
    res <- query conn "SELECT rowid FROM point WHERE state = ? AND patches = ?" v
    case res of
        [] -> do
            execute conn "INSERT INTO point VALUES (?,?)" v
            [Only x] <- query conn "SELECT rowid FROM point WHERE state = ? AND patches = ?" v
            return x
        [Only x] -> return x
        _ -> error $ "ensurePoint, multiple points found"


storeUpdate :: Store -> [Update] -> IO Store
storeUpdate store xs = do
    now <- getCurrentTime
    print $ ("Updating",xs)
    res <- foldM (f now) store xs
    print "Updated"
    return res
    where
        execute a b c = do
            print b
            Database.SQLite.Simple.execute a b c

        f now store@Store{..} x = case x of
            IUState s p -> do
                pt <- maybe (return Nothing) (fmap Just . ensurePoint store) p
                execute conn "INSERT INTO state VALUES (?,?,?)" $ DbState s now pt
                return store{stateMap = Map.insert s (StateInfo now p) stateMap}
            IUQueue p a -> do
                execute conn "INSERT INTO patch VALUES (?,?,?,?,?,?,?,?,?)" $
                    DbPatch p a now Nothing Nothing Nothing Nothing Nothing Nothing
                return store{patchInfo = Map.insert p (PatchInfo (now,a) Nothing Nothing Nothing Nothing Nothing Nothing) patchInfo}
            IUStart p -> do
                execute conn "UPDATE patch SET start=? WHERE patch=?" (now, p)
                return store{patchInfo = Map.adjust (\pa -> pa{paStart = Just now}) p patchInfo}
            IUPlausible p -> do
                execute conn "UPDATE patch SET plausible=? WHERE patch=?" (now, p)
                return store{patchInfo = Map.adjust (\pa -> pa{paPlausible = Just now}) p patchInfo}
            IUMerge p -> do
                execute conn "UPDATE patch SET merge=? WHERE patch=?" (now, p)
                return store{patchInfo = Map.adjust (\pa -> pa{paMerge = Just now}) p patchInfo}
            IUDelete p -> do
                execute conn "UPDATE patch SET delete_=? WHERE patch=?" (now, p)
                return store{patchInfo = Map.adjust (\pa -> pa{paDelete = Just now}) p patchInfo}
            IUSupersede p -> do
                execute conn "UPDATE patch SET supersede=? WHERE patch=?" (now, p)
                return store{patchInfo = Map.adjust (\pa -> pa{paSupersede = Just now}) p patchInfo}
            IUReject p t pt -> do
                pt2 <- ensurePoint store pt
                [Only run] <- query conn "SELECT rowid FROM run WHERE success=? AND point=? AND test=?" (False, pt2, t)
                execute conn "UPDATE patch SET reject=? WHERE patch=?" (now, p)
                execute conn "INSERT INTO reject VALUES (?,?,?)" $ DbReject p t run
                let add Nothing = (now, Map.singleton t pt)
                    add (Just (a,b)) = (a, b `Map.union` Map.singleton t pt)
                return store{patchInfo = Map.adjust (\pa -> pa{paReject = Just $ add $ paReject pa}) p patchInfo}
            PUTest p t -> do
                pt <- ensurePoint store p
                res :: [Only (Maybe Test)] <- query conn "SELECT test FROM test WHERE point=?" $ Only pt
                if null res then
                    forM_ t $ \t -> execute conn "INSERT INTO test VALUES (?,?)" (pt, t)
                else
                    when (Set.fromList (mapMaybe fromOnly res) /= Set.fromList t) $
                        error "Test disagreement"
                return store{points = Map.insertWith mappend p mempty{poTodo=Just $ Set.fromList t} points}
            PUPass p t -> do
                pt <- ensurePoint store p
                execute conn "INSERT INTO run VALUES (?,?,?,?,?,?)" $ DbRun pt t True (Client "") now 0
                return store{points = Map.insertWith mappend p mempty{poPass=Set.singleton t} points}
            PUFail p t -> do
                pt <- ensurePoint store p
                execute conn "INSERT INTO run VALUES (?,?,?,?,?,?)" $ DbRun pt t False (Client "") now 0
                return store{points = Map.insertWith mappend p mempty{poFail=Set.singleton t} points}


storeSaveUpdate :: Store -> State -> Answer -> IO ()
storeSaveUpdate _ _ _ = return ()

storeLoadUpdate :: Store -> State -> IO [Answer]
storeLoadUpdate _ _ = return []

storeSaveTest :: Store -> Question -> Answer -> IO ()
storeSaveTest _ _ _ = return ()

storeLoadTest :: Store -> (State, [Patch]) -> Maybe Test -> IO [(Question, Answer)]
storeLoadTest _ _ _ = return []
