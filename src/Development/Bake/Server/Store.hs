{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

-- Stuff on disk on the server
module Development.Bake.Server.Store(
    Store, newStore,
    PatchInfo(..), paAlive, storePatchList, storeIsPatch, storePatch, storeAlive,
    PointInfo(..), poTest, storePoint, storeSupersetPass,
    StateInfo(..), storeStateList, storeState,
    RunId, storeRunList, storeStateFile, storeRunFile,
    storeItemsDate,
    storeSkip,
    storeExtra, storeExtraAdd,
    Update(..), storeUpdate
    ) where

import Development.Bake.Server.Database
import General.Database
import Development.Bake.Core.Type
import Development.Bake.Core.Message
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Time
import Data.Char
import Data.List.Extra
import Data.String
import System.IO.Unsafe
import Data.IORef
import Data.Monoid
import Data.Maybe
import Data.Tuple.Extra
import Control.Applicative
import Control.Monad.Extra
import System.Directory
import Database.SQLite.Simple hiding (NamedParam(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.FilePath
import Control.DeepSeq
import Control.Exception
import Prelude


data Cache = Cache
    {cachePatch :: HashMap.HashMap Patch (PatchId, PatchInfo)
    ,cacheState :: HashMap.HashMap State (StateId, StateInfo)
    ,cachePoint :: HashMap.HashMap Point (PointId, PointInfo)
    ,cacheExtra :: HashMap.HashMap (Either State Patch) (Maybe T.Text)
    ,cacheSkip :: Maybe (Map.Map Test String)
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
    ,paReject :: Maybe (UTCTime, Map.Map (Maybe Test) (State, [Patch]))
    ,paPlausible :: Maybe UTCTime
    ,paMerge :: Maybe UTCTime
    }
    deriving Show

paAlive :: PatchInfo -> Bool
paAlive PatchInfo{..} = isNothing paDelete && isNothing paSupersede && isNothing paReject && isNothing paMerge

data StateInfo = StateInfo
    {stCreated :: UTCTime
    ,stSource :: Maybe Point
    }

data Store = Store
    {conn :: Connection
    ,path :: FilePath
    ,cache :: IORef Cache
    }

newStore :: Bool -> FilePath -> IO Store
newStore mem path = do
    time <- getCurrentTime
    createDirectoryIfMissing True path
    conn <- create $ if mem then ":memory:" else path </> "bake.sqlite"
    cache <- newIORef $ Cache HashMap.empty HashMap.empty HashMap.empty HashMap.empty Nothing
    return $ Store conn path cache

storePoint :: Store -> Point -> PointInfo
storePoint store = snd . unsafePerformIO . storePointEx store

{-# NOINLINE computePointEx #-}
computePointEx store@Store{..} x = do
    pt <- ensurePoint store x
    tests <- query conn "SELECT test FROM test WHERE point IS ?" $ Only pt
    pass <- query conn "SELECT test FROM run WHERE point IS ? AND success IS 1" $ Only pt
    fail <- query conn "SELECT test FROM run WHERE point IS ? AND success IS 0" $ Only pt
    return $ (,) pt $ PointInfo
        (if null tests then Nothing else Just $ Set.fromList $ mapMaybe fromOnly tests)
        (Set.fromList $ map fromOnly pass) (Set.fromList $ map fromOnly fail)


storePointEx :: Store -> (State,[Patch]) -> IO (PointId, PointInfo)
storePointEx store@Store{..} x = do
    c <- readIORef cache
    case HashMap.lookup x $ cachePoint c of
        Just res -> return res
        _ -> do
            res <- computePointEx store x
            modifyIORef cache $ \c -> c{cachePoint = HashMap.insert x res $ cachePoint c}
            return res

storePatchList :: Store -> [Patch]
storePatchList store@Store{..} = unsafePerformIO $ do
    ps <- query_ conn "SELECT patch FROM patch"
    return $ map fromOnly ps

storeIsPatch :: Store -> Patch -> Bool
storeIsPatch store@Store{..} p = unsafePerformIO $ do
    ps :: [Only Int] <- query conn "SELECT 1 FROM patch WHERE patch IS ?" $ Only p
    return $ ps /= []

storePatch :: Store -> Patch -> PatchInfo
storePatch store = snd . unsafePerformIO . storePatchEx store

storePatchEx :: Store -> Patch -> IO (PatchId, PatchInfo)
storePatchEx store@Store{..} p = do
    let ans = do
            [(row, paAuthor, paQueue, paStart, paDelete, paSupersede, paReject, paPlausible, paMerge)] <-
                sqlSelect conn (pcId,pcAuthor,pcQueue,pcStart,pcDelete,pcSupersede,pcReject,pcPlausible,pcMerge)
                [pcPatch %== p]
            reject <- if isNothing paReject then return Nothing else do
                ts <- query conn "SELECT DISTINCT reject.test, run.point FROM reject, run WHERE reject.patch IS ? AND reject.run IS run.rowid" $ Only (row :: PatchId)
                ts <- mapM (\(a,b) -> (a,) <$> unsurePoint store b) ts
                return (Just (fromJust paReject, Map.fromList ts))
            return (row, PatchInfo (paQueue, paAuthor) paStart paDelete paSupersede reject paPlausible paMerge)
    c <- readIORef cache
    case HashMap.lookup p $ cachePatch c of
        Just res -> return res
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cachePatch = HashMap.insert p res $ cachePatch c}
            return res

storeState :: Store -> State -> StateInfo
storeState store = snd . unsafePerformIO . storeStateEx store

storeStateList :: Store -> [State]
storeStateList store@Store{..} = unsafePerformIO $ do
    ss <- query_ conn "SELECT state FROM state"
    return $ map fromOnly ss

storeStateEx :: Store -> State -> IO (StateId, StateInfo)
storeStateEx store@Store{..} st = do
    let ans = do
            [(row, sCreate, sPoint)] <- sqlSelect conn (stId, stCreate, stPoint) [stState %== st]
            pt <- maybe (return Nothing) (fmap Just . unsurePoint store) sPoint
            return (row, StateInfo sCreate pt)
    c <- readIORef cache
    case HashMap.lookup st $ cacheState c of
        Just res -> return res
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cacheState = HashMap.insert st res $ cacheState c}
            return res


storeItemsDate :: Store -> (UTCTime, UTCTime) -> [Either State Patch]
storeItemsDate store (start, end) = reverse $ merge
    (sortOn (stCreated . snd) $ map (id &&& storeState store) $ storeStateList store)
    (sortOn (paQueued . snd) $ map (id &&& storePatch store) $ storePatchList store)
    where
        merge (s:ss) o@(span (isJust . paReject . snd) -> (reject, p:ps))
            | stCreated (snd s) < paMaxTime (snd p) = Left (fst s) : merge ss o
            | otherwise = map (Right . fst) (reject ++ [p]) ++ merge (s:ss) ps
        merge ss ps = map (Left . fst) ss ++ map (Right . fst) ps

        paMaxTime PatchInfo{..} = maximum $ fst paQueued : catMaybes [paStart,paDelete,paSupersede,fmap fst paReject,paPlausible,paMerge]

storeSkip :: Store -> Map.Map Test String
storeSkip Store{..} = unsafePerformIO $ do
    let ans = do
            Map.fromList <$> sqlSelect conn (skTest, skComment) []
    c <- readIORef cache
    case cacheSkip c of
        Just res -> return res
        _ -> do
            res <- ans
            modifyIORef cache $ \c -> c{cacheSkip = Just res}
            return res

storeAlive :: Store -> Set.Set Patch
storeAlive Store{..} = unsafePerformIO $ do
    ps <- query_ conn "SELECT patch FROM patch WHERE delete_ IS NULL AND supersede IS NULL AND reject IS NULL AND merge IS NULL"
    return $ Set.fromList $ map fromOnly ps

storeSupersetPass :: Store -> (State,[Patch]) -> Set.Set Test
storeSupersetPass store@Store{..} (s,ps) = unsafePerformIO $ do
    s <- ensureState store s
    ps <- mapM (ensurePatch store) ps
    let q = unlines
            ["SELECT DISTINCT test FROM"
            ,"(SELECT run.test AS test, min(run.success) AS success"
            ,"FROM run, point"
            ,"WHERE run.point IS point.rowid AND run.test IS NOT NULL AND point.state IS ? AND point.patches LIKE ?"
            ,"GROUP BY run.point, run.test)"
            ,"WHERE success IS 1"]
    ts <- query conn (fromString q) (s, patchIdsSuperset ps)
    return $ Set.fromList $ map fromOnly ts

data Update
    = IUState State Answer (Maybe Point) -- assumed to be success
    | IUQueue Patch Author
    | IUStart Patch
    | IUDelete Patch
    | IUReject Patch (Maybe Test) Point
    | IUPlausible Patch
    | IUSupersede Patch
    | IUMerge Patch
    | SUAdd Test String
    | SUDel Test
    | PURun UTCTime Question Answer
      deriving Show

unsureState :: Store -> StateId -> IO State
unsureState Store{..} s = do
    [Only s] <- sqlSelect conn (Only stState) [stId %== s]
    return s

unsurePatch :: Store -> PatchId -> IO Patch
unsurePatch Store{..} p = do
    [Only p] <- query conn "SELECT patch FROM patch WHERE rowid IS ?" (Only p)
    return p

unsurePoint :: Store -> PointId -> IO Point
unsurePoint store@Store{..} pt = do
    [(state, patches)] <- sqlSelect conn (ptState, ptPatches) [ptId %== pt]
    liftM2 (,) (unsureState store state) (mapM (unsurePatch store) $ fromPatchIds patches)

ensureState :: Store -> State -> IO StateId
ensureState Store{..} s = do
    [Only s] <- sqlSelect conn (Only stId) [stState %== s]
    return s

ensurePatch :: Store -> Patch -> IO PatchId
ensurePatch store = fmap fst . storePatchEx store

ensurePoint :: Store -> Point -> IO PointId
ensurePoint store@Store{..} (s, ps) = do
    s <- ensureState store s
    ps <- patchIds <$> mapM (ensurePatch store) ps
    res <- sqlSelect conn ptId [ptState %== s, ptPatches %== ps]
    case res of
        [] -> sqlInsert conn ptTable (s, ps)
        [Only x] -> return x
        _ -> error $ "ensurePoint, multiple points found"


-- don't inline so GHC can't tell the store is returned unchanged
{-# NOINLINE storeUpdate #-}
storeUpdate :: Store -> [Update] -> IO Store
storeUpdate store xs = do
    -- important so that if the updates depend on the current store they are forced first
    -- the perils of impurity!
    evaluate $ rnf $ show xs
    now <- getCurrentTime
--    print $ ("Updating",xs)
    mapM_ (f now store) xs
    cache <- newIORef $ Cache HashMap.empty HashMap.empty HashMap.empty HashMap.empty Nothing
    -- print "Updated"
    return store{cache=cache}
    where
        execute a b c = do
            -- print b
            Database.SQLite.Simple.execute a b c

        f now store@Store{..} x = case x of
            IUState s Answer{..} p -> do
                pt <- maybe (return Nothing) (fmap Just . ensurePoint store) p
                prev <- sqlSelect conn stId [stState %== s]
                x <- case prev of
                    [] ->
                        sqlInsert conn stTable (s,now,pt,aDuration)
                    Only x:_ -> do
                        sqlUpdate conn [stCreate := now, stPoint := pt, stDuration := aDuration] [stId %== x]
                        return x
                createDirectoryIfMissing True (path </> show x)
                TL.writeFile (path </> show x </> "update.txt") aStdout
                modifyIORef cache $ \c -> c{cacheState = HashMap.insert s (x, StateInfo now p) $ cacheState c}
            IUQueue p a -> do
                x <- sqlInsert conn pcTable (p,a,now,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)
                modifyIORef cache $ \c -> c{cachePatch = HashMap.insert p (x, PatchInfo (now,a) Nothing Nothing Nothing Nothing Nothing Nothing) $ cachePatch c}
            IUStart p -> do
                execute conn "UPDATE patch SET start=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = HashMap.adjust (second $ \pa -> pa{paStart = Just now}) p $ cachePatch c}
            IUPlausible p -> do
                execute conn "UPDATE patch SET plausible=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = HashMap.adjust (second $ \pa -> pa{paPlausible = Just now}) p $ cachePatch c}
            IUMerge p -> do
                execute conn "UPDATE patch SET merge=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = HashMap.adjust (second $ \pa -> pa{paMerge = Just now}) p $ cachePatch c}
            IUDelete p -> do
                execute conn "UPDATE patch SET delete_=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = HashMap.adjust (second $ \pa -> pa{paDelete = Just now}) p $ cachePatch c}
            IUSupersede p -> do
                execute conn "UPDATE patch SET supersede=? WHERE patch IS ?" (now, p)
                modifyIORef cache $ \c -> c{cachePatch = HashMap.adjust (second $ \pa -> pa{paSupersede = Just now}) p $ cachePatch c}
            IUReject p t pt -> do
                pt2 <- ensurePoint store pt
                [Only run] <- query conn "SELECT rowid FROM run WHERE success IS ? AND point IS ? AND test IS ?" (False, pt2, t)
                execute conn "UPDATE patch SET reject=? WHERE patch IS ?" (now, p)
                pa <- ensurePatch store p
                sqlInsert conn rjTable (pa, t, run)
                let add Nothing = (now, Map.singleton t pt)
                    add (Just (a,b)) = (a, b `Map.union` Map.singleton t pt)
                modifyIORef cache $ \c -> c{cachePatch = HashMap.adjust (second $ \pa -> pa{paReject = Just $ add $ paReject pa}) p $ cachePatch c}
            SUAdd t msg -> do
                sqlInsert conn skTable (t, msg)
                modifyIORef cache $ \c -> c{cacheSkip = Map.insert t msg <$> cacheSkip c}
            SUDel t -> do
                sqlDelete conn skTable [skTest %== t]
                modifyIORef cache $ \c -> c{cacheSkip = Map.delete t <$> cacheSkip c}
            PURun t Question{..} Answer{..} -> do
                let together (x1,y1) (x2,y2) = (x1, y1 <> y2)
                pt <- ensurePoint store qCandidate
                when (qTest == Nothing) $ do
                    res :: [Only (Maybe Test)] <- sqlSelect conn tsTest [tsPoint %== pt]
                    if null res then do
                        sqlInsert conn tsTable (pt, Nothing)
                        forM_ aTests $ \t -> sqlInsert conn tsTable (pt, Just t)
                    else
                        when (Set.fromList (mapMaybe fromOnly res) /= Set.fromList aTests) $
                            error "Test disagreement"
                    modifyIORef cache $ \c -> c{cachePoint = HashMap.insertWith together qCandidate (pt, mempty{poTodo=Just $ Set.fromList aTests}) $ cachePoint c}
                x <- sqlInsert conn rnTable (pt,qTest,aSuccess,qClient,t,aDuration)
                createDirectoryIfMissing True $ path </> show pt
                TL.writeFile (path </> show pt </> show x ++ "-" ++ maybe "Prepare" (safely . fromTest) qTest <.> "txt") aStdout
                let val = if aSuccess then mempty{poPass=Set.singleton qTest} else mempty{poFail=Set.singleton qTest}
                modifyIORef cache $ \c -> c{cachePoint = HashMap.insertWith together qCandidate (pt, val) $ cachePoint c}

safely :: String -> String
safely = map f . take 100
    where f x | isAlphaNum x || x `elem` (".-_" :: String) = x
          f x = '_'


storeStateFile :: Store -> State -> Maybe String
storeStateFile store@Store{..} st = unsafePerformIO $ do
    st <- ensureState store st
    let file = path </> show st </> "update.txt"
    ifM (doesFileExist file) (Just <$> readFile file) (return Nothing)


storeRunList :: Store -> Maybe Client -> Maybe (Maybe Test) -> Maybe State -> [Patch] -> Maybe RunId -> [(RunId, UTCTime, Question, Answer)]
storeRunList store@Store{..} client test state patches run = unsafePerformIO $ do
    point <- maybe (return Nothing) (fmap Just . ensurePoint store . (, patches)) state
    patches <- if isNothing state && patches /= [] then Just <$> mapM (ensurePatch store) patches else return Nothing
    let filt = [rnClient %== x | Just x <- [client]] ++
               [rnTest %== x | Just x <- [test]] ++
               [ptId %==% rnPoint %&& likeP ptPatches (patchIdsSuperset x) | Just x <- [patches]] ++
               [rnPoint %== x | Just x <- [point]] ++
               [rnId %== x | Just x <- [run]]
    xs <- sqlSelect conn (rnId, rnPoint, rnTest, rnSuccess, rnClient, rnStart, rnDuration) (orderDatetimeDesc rnStart : filt)
    forM xs $ \(rnId, rnPoint, rnTest, rnSuccess, rnClient, rnStart, rnDuration) -> do
        pt <- unsurePoint store rnPoint
        return (rnId, rnStart, Question pt rnTest 0 rnClient, Answer mempty rnDuration [] rnSuccess)


storeRunFile :: Store -> RunId -> Maybe String
storeRunFile store@Store{..} run = unsafePerformIO $ do
    [(rPoint, rTest)] <- sqlSelect conn (rnPoint, rnTest) [rnId %== run]
    let file = path </> show rPoint </> show run ++ "-" ++ maybe "Prepare" (safely . fromTest) rTest <.> "txt"
    ifM (doesFileExist file) (Just <$> readFile file) (return Nothing)


storeExtraFile :: Store -> Either State Patch -> IO FilePath
storeExtraFile store@Store{..} x = (path </>) <$> either (fmap show . ensureState store) (fmap show . ensurePatch store) x


storeExtra :: Store -> Either State Patch -> Maybe (String, String)
storeExtra store@Store{..} sp = unsafePerformIO $ do
    c <- readIORef cache
    prefix <- storeExtraFile store sp
    short <- case HashMap.lookup sp $ cacheExtra c of
        Just v -> return v
        Nothing -> do
            short <- ifM (doesFileExist $ prefix </> "extra-short.html") (fmap Just $ T.readFile $ prefix </> "extra-short.html") (return Nothing)
            modifyIORef cache $ \c -> c{cacheExtra = HashMap.insert sp short $ cacheExtra c}
            return short
    case short of
        Nothing -> return Nothing
        Just short -> do
            long <- unsafeInterleaveIO $ readFile $ prefix </> "extra-long.html"
            return $ Just (T.unpack short, long)


storeExtraAdd :: Store -> Either State Patch -> (T.Text, TL.Text) -> IO ()
storeExtraAdd store@Store{..} sp (short, long) = do
    prefix <- storeExtraFile store sp
    createDirectoryIfMissing True prefix
    T.writeFile (prefix </> "extra-short.html") short
    TL.writeFile (prefix </> "extra-long.html") long
    modifyIORef cache $ \c -> c{cacheExtra = HashMap.insert sp (Just short) $ cacheExtra c}
