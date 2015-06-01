{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, TupleSections #-}

-- Stuff on disk on the server
module Development.Bake.Server.Store(
    Store, newStore,
    PatchInfo(..), storePatchList, storeIsPatch, storePatch, storeAlive,
    PointInfo(..), poTest, storePoint, storeSupersetPass,
    StateInfo(..), storeState,
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
import Data.String
import System.IO.Unsafe
import Data.IORef
import Data.Monoid
import Data.Maybe
import Data.Tuple.Extra
import Control.Monad
import System.Directory
import Database.SQLite.Simple
import System.FilePath


data Cache = Cache
    {cachePatch :: Map.Map Patch (PatchId, PatchInfo)
    ,cacheState :: Map.Map State (StateId, StateInfo)
    ,cachePoint :: Map.Map (State, [Patch]) (Point, PointInfo)
    ,cacheAlive :: Maybe (Set.Set Patch)
    ,cacheSuperset :: Maybe ((State, [Patch]), Set.Set Test)
    }


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
    ,paReject :: Maybe (UTCTime, Map.Map (Maybe Test) ())
    ,paPlausible :: Maybe UTCTime
    ,paMerge :: Maybe UTCTime
    }
    deriving Show

data StateInfo = StateInfo
    {stCreated :: UTCTime
    ,stSource :: Maybe (State, [Patch])
    }

data Store = Store
    {conn :: IORef Connection
    ,path :: FilePath
    ,cache :: IORef Cache
    }

newStore :: Bool -> FilePath -> IO Store
newStore mem path = do
    time <- getCurrentTime
    createDirectoryIfMissing True path
    conn <- create $ if mem then ":memory:" else path </> "bake.sqlite"
    execute_ conn "PRAGMA journal_mode = WAL;"
    execute_ conn "PRAGMA synchronous = OFF;"
    conn <- newIORef conn
    cache <- newIORef $ Cache Map.empty Map.empty Map.empty Nothing Nothing
    return $ Store conn path cache

storePoint :: Store -> (State, [Patch]) -> PointInfo
storePoint store = snd . unsafePerformIO . storePointEx store

storePointEx :: Store -> (State,[Patch]) -> IO (Point, PointInfo)
storePointEx store@Store{..} x = do
    let ans = do
            pt <- ensurePoint store x
            conn <- readIORef conn
            tests <- query conn "SELECT test FROM test WHERE point IS ?" $ Only pt
            pass <- query conn "SELECT test FROM run WHERE point IS ? AND success IS 1" $ Only pt
            fail <- query conn "SELECT test FROM run WHERE point IS ? AND success IS 0" $ Only pt
            return $ (,) pt $ PointInfo
                (if null tests then Nothing else Just $ Set.fromList $ mapMaybe fromOnly tests)
                (Set.fromList $ map fromOnly pass) (Set.fromList $ map fromOnly fail)
    c <- readIORef cache
    case Map.lookup x $ cachePoint c of
        Just res -> return res
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cachePoint = Map.insert x res $ cachePoint c}
            return res

storePatchList :: Store -> [Patch]
storePatchList store@Store{..} = unsafePerformIO $ do
    conn <- readIORef conn
    ps <- query_ conn "SELECT patch FROM patch"
    return $ map fromOnly ps

storeIsPatch :: Store -> Patch -> Bool
storeIsPatch store@Store{..} p = unsafePerformIO $ do
    conn <- readIORef conn
    ps :: [Only Int] <- query conn "SELECT 1 FROM patch WHERE patch IS ?" $ Only p
    return $ ps /= []

storePatch :: Store -> Patch -> PatchInfo
storePatch store = snd . unsafePerformIO . storePatchEx store

storePatchEx :: Store -> Patch -> IO (PatchId, PatchInfo)
storePatchEx store@Store{..} p = do
    let ans = do
            conn <- readIORef conn
            [Only row :. DbPatch{..}] <- query conn "SELECT rowid, * FROM patch WHERE patch IS ?" $ Only p
            reject <- if isNothing pReject then return Nothing else do
                ts <- query conn "SELECT test FROM reject WHERE patch IS ?" $ Only (row :: PatchId)
                return (Just (fromJust pReject, Map.fromList $ map (,()) $ map fromOnly ts))
            return (row, PatchInfo (pQueue, pAuthor) pStart pDelete pSupersede reject pPlausible pMerge)
    c <- readIORef cache
    case Map.lookup p $ cachePatch c of
        Just res -> return res
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cachePatch = Map.insert p res $ cachePatch c}
            return res

storeState :: Store -> State -> StateInfo
storeState store = snd . unsafePerformIO . storeStateEx store

storeStateEx :: Store -> State -> IO (StateId, StateInfo)
storeStateEx store@Store{..} st = do
    let ans = do
            conn <- readIORef conn
            [Only row :. DbState{..}] <- query conn "SELECT rowid, * FROM state WHERE state IS ?" $ Only st
            pt <- maybe (return Nothing) (fmap Just . unsurePoint store) sPoint
            return (row, StateInfo sCreate pt)
    c <- readIORef cache
    case Map.lookup st $ cacheState c of
        Just res -> return res
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cacheState = Map.insert st res $ cacheState c}
            return res


storeAlive :: Store -> Set.Set Patch
storeAlive Store{..} = unsafePerformIO $ do
    let ans = do
            conn <- readIORef conn
            ps <- query_ conn "SELECT patch FROM patch WHERE delete_ IS NULL AND supersede IS NULL AND reject IS NULL AND merge IS NULL"
            return $ Set.fromList $ map fromOnly ps
    c <- readIORef cache
    case cacheAlive c of
        Just s | False -> return s
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cacheAlive=Just res}
            return res

storeSupersetPass :: Store -> (State,[Patch]) -> Set.Set Test
storeSupersetPass store@Store{..} (s,ps) = unsafePerformIO $ do
    let ans = do
            s <- ensureState store s
            ps <- mapM (ensurePatch store) ps
            let q = unlines
                    ["SELECT DISTINCT test FROM"
                    ,"(SELECT run.test AS test, min(run.success) AS success"
                    ,"FROM run, point"
                    ,"WHERE run.point IS point.rowid AND run.test IS NOT NULL AND point.state IS ? AND point.patches LIKE ?"
                    ,"GROUP BY run.point, run.test)"
                    ,"WHERE success == 1"]
            conn <- readIORef conn
            ts <- query conn (fromString q) (s, patchIdsSuperset ps)
            return $ Set.fromList $ map fromOnly ts
    c <- readIORef cache
    case cacheSuperset c of
        Just (sps, res) | False, sps == (s,ps) -> return res
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cacheSuperset = Just ((s,ps),res)}
            return res

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

unsureState :: Store -> StateId -> IO State
unsureState Store{..} s = do
    conn <- readIORef conn
    [Only s] <- query conn "SELECT state FROM state WHERE rowid IS ?" (Only s)
    return s

unsurePatch :: Store -> PatchId -> IO Patch
unsurePatch Store{..} p = do
    conn <- readIORef conn
    [Only p] <- query conn "SELECT patch FROM patch WHERE rowid IS ?" (Only p)
    return p

unsurePoint :: Store -> Point -> IO (State, [Patch])
unsurePoint store@Store{..} pt = do
    conn <- readIORef conn
    [DbPoint{..}] <- query conn "SELECT * FROM point WHERE rowid IS ?" (Only pt)
    liftM2 (,) (unsureState store tState) (mapM (unsurePatch store) $ fromPatchIds tPatches)

ensureState :: Store -> State -> IO StateId
ensureState Store{..} s = do
    conn <- readIORef conn
    [Only s] <- query conn "SELECT rowid FROM state WHERE state IS ?" (Only s)
    return s

ensurePatch :: Store -> Patch -> IO PatchId
ensurePatch store = fmap fst . storePatchEx store

ensurePoint :: Store -> (State, [Patch]) -> IO Point
ensurePoint store@Store{..} (s, ps) = do
    s <- ensureState store s
    ps <- mapM (ensurePatch store) ps
    let v = DbPoint s (patchIds ps)
    conn <- readIORef conn
    res <- query conn "SELECT rowid FROM point WHERE state IS ? AND patches IS ?" v
    case res of
        [] -> do
            execute conn "INSERT INTO point VALUES (?,?)" v
            [Only x] <- query_ conn "SELECT last_insert_rowid()"
            return x
        [Only x] -> return x
        _ -> error $ "ensurePoint, multiple points found"


storeUpdate :: Store -> [Update] -> IO Store
storeUpdate store xs = do
    now <- getCurrentTime
    c <- readIORef $ conn store
--    print $ ("Updating",xs)
    mapM_ (f now c store) xs
    -- print "Updated"
    writeIORef (conn store) $ error "Store must be used linearly, you are using an old connection"
    conn <- newIORef c
    return store{conn = conn}
    where
        execute a b c = do
            -- print b
            Database.SQLite.Simple.execute a b c

        f now conn store@Store{conn=_,..} x = case x of
            IUState s p -> do
                pt <- maybe (return Nothing) (fmap Just . ensurePoint store) p
                execute conn "INSERT INTO state VALUES (?,?,?)" $ DbState s now pt
                [Only x] <- query_ conn "SELECT last_insert_rowid()"
                modifyIORef cache $ \c -> c{cacheState = Map.insert s (x, StateInfo now p) $ cacheState c}
            IUQueue p a -> do
                execute conn "INSERT INTO patch VALUES (?,?,?,?,?,?,?,?,?)" $
                    DbPatch p a now Nothing Nothing Nothing Nothing Nothing Nothing
                [Only x] <- query_ conn "SELECT last_insert_rowid()"
                modifyIORef cache $ \c -> c{cachePatch = Map.insert p (x, PatchInfo (now,a) Nothing Nothing Nothing Nothing Nothing Nothing) $ cachePatch c}
            IUStart p -> do
                execute conn "UPDATE patch SET start=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = Map.adjust (second $ \pa -> pa{paStart = Just now}) p $ cachePatch c}
            IUPlausible p -> do
                execute conn "UPDATE patch SET plausible=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = Map.adjust (second $ \pa -> pa{paPlausible = Just now}) p $ cachePatch c}
            IUMerge p -> do
                execute conn "UPDATE patch SET merge=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = Map.adjust (second $ \pa -> pa{paMerge = Just now}) p $ cachePatch c}
            IUDelete p -> do
                execute conn "UPDATE patch SET delete_=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = Map.adjust (second $ \pa -> pa{paDelete = Just now}) p $ cachePatch c}
            IUSupersede p -> do
                execute conn "UPDATE patch SET supersede=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = Map.adjust (second $ \pa -> pa{paSupersede = Just now}) p $ cachePatch c}
            IUReject p t pt -> do
                pt2 <- ensurePoint store pt
                [Only run] <- query conn "SELECT rowid FROM run WHERE success IS ? AND point IS ? AND test IS ?" (False, pt2, t)
                execute conn "UPDATE patch SET reject=? WHERE patch IS ?" (now, p)
                pa <- ensurePatch store p
                execute conn "INSERT INTO reject VALUES (?,?,?)" $ DbReject pa t run
                let add Nothing = (now, Map.singleton t ())
                    add (Just (a,b)) = (a, b `Map.union` Map.singleton t ())
                modifyIORef cache $ \c -> c{cachePatch = Map.adjust (second $ \pa -> pa{paReject = Just $ add $ paReject pa}) p $ cachePatch c}
            PUTest p t -> do
                pt <- ensurePoint store p
                res :: [Only (Maybe Test)] <- query conn "SELECT test FROM test WHERE point=?" $ Only pt
                if null res then
                    forM_ t $ \t -> execute conn "INSERT INTO test VALUES (?,?)" (pt, t)
                else
                    when (Set.fromList (mapMaybe fromOnly res) /= Set.fromList t) $
                        error "Test disagreement"
                modifyIORef cache $ \c -> c{cachePoint = Map.insertWith together p (pt, mempty{poTodo=Just $ Set.fromList t}) $ cachePoint c}
            PUPass p t -> do
                pt <- ensurePoint store p
                execute conn "INSERT INTO run VALUES (?,?,?,?,?,?)" $ DbRun pt t True (Client "") now 0
                modifyIORef cache $ \c -> c{cachePoint = Map.insertWith together p (pt, mempty{poPass=Set.singleton t}) $ cachePoint c}
            PUFail p t -> do
                pt <- ensurePoint store p
                execute conn "INSERT INTO run VALUES (?,?,?,?,?,?)" $ DbRun pt t False (Client "") now 0
                modifyIORef cache $ \c -> c{cachePoint = Map.insertWith together p (pt, mempty{poFail=Set.singleton t}) $ cachePoint c}

        together (x1,y1) (x2,y2) = (x1, y1 <> y2)


storeSaveUpdate :: Store -> State -> Answer -> IO ()
storeSaveUpdate _ _ _ = return ()

storeLoadUpdate :: Store -> State -> IO [Answer]
storeLoadUpdate _ _ = return []

storeSaveTest :: Store -> Question -> Answer -> IO ()
storeSaveTest _ _ _ = return ()

storeLoadTest :: Store -> (State, [Patch]) -> Maybe Test -> IO [(Question, Answer)]
storeLoadTest _ _ _ = return []
