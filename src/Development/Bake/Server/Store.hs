{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

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
    tests <- sqlSelect conn tsTest [tsPoint %== pt]
    pass <- sqlSelect conn rnTest [distinct rnTest, rnPoint %== pt, rnSuccess %== True]
    fail <- sqlSelect conn rnTest [distinct rnTest, rnPoint %== pt, rnSuccess %== False]
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
    ps <- sqlSelect conn pcPatch []
    return $ map fromOnly ps

storeIsPatch :: Store -> Patch -> Bool
storeIsPatch store@Store{..} p = unsafePerformIO $ do
    ps <- sqlSelect conn pcPatch [pcPatch %== p]
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
                ts <- sqlSelect conn (rjTest, rnPoint) [distinct rjTest, rjPatch %== row, rjRun %==% rnId]
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
    ss <- sqlSelect conn stState []
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
    ps <- sqlSelect conn pcPatch [nullP pcDelete, nullP pcSupersede, nullP pcReject, nullP pcMerge]
    return $ Set.fromList $ map fromOnly ps

storeSupersetPass :: Store -> (State,[Patch]) -> Set.Set Test
storeSupersetPass store@Store{..} (s,ps) = unsafePerformIO $ do
    s <- ensureState store s
    ps <- mapM (ensurePatch store) ps
    let f success = do
        xs <- sqlSelect conn rnTest [rnPoint %==% ptId, ptState %== s, likeP ptPatches $ patchIdsSuperset ps, rnSuccess %== success]
        return $ Set.fromList $ mapMaybe fromOnly xs
    liftM2 Set.difference (f True) (f False)

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
    [Only p] <- sqlSelect conn pcPatch [pcId %== p]
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
    mapM_ (f now store) xs
    cache <- newIORef $ Cache HashMap.empty HashMap.empty HashMap.empty HashMap.empty Nothing
    return store{cache=cache}
    where
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
            IUQueue p a -> do
                void $ sqlInsert conn pcTable (p,a,now,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)
            IUStart p -> do
                sqlUpdate conn [pcStart := Just now] [pcPatch %== p]
            IUPlausible p -> do
                sqlUpdate conn [pcPlausible := Just now] [pcPatch %== p]
            IUMerge p -> do
                sqlUpdate conn [pcMerge := Just now] [pcPatch %== p]
            IUDelete p -> do
                sqlUpdate conn [pcDelete := Just now] [pcPatch %== p]
            IUSupersede p -> do
                sqlUpdate conn [pcSupersede := Just now] [pcPatch %== p]
            IUReject p t pt -> do
                pt2 <- ensurePoint store pt
                Only run:_ <- sqlSelect conn rnId [rnSuccess %== False, rnPoint %== pt2, rnTest %== t]
                sqlUpdate conn [pcReject := Just now] [pcPatch %== p]
                pa <- ensurePatch store p
                void $ sqlInsert conn rjTable (pa, t, run)
            SUAdd t msg -> do
                void $ sqlInsert conn skTable (t, msg)
            SUDel t -> do
                sqlDelete conn skTable [skTest %== t]
            PURun t Question{..} Answer{..} -> do
                pt <- ensurePoint store qCandidate
                when (qTest == Nothing) $ do
                    res :: [Only (Maybe Test)] <- sqlSelect conn tsTest [tsPoint %== pt]
                    if null res then do
                        sqlInsert conn tsTable (pt, Nothing)
                        forM_ aTests $ \t -> sqlInsert conn tsTable (pt, Just t)
                    else
                        when (Set.fromList (mapMaybe fromOnly res) /= Set.fromList aTests) $
                            error "Test disagreement"
                x <- sqlInsert conn rnTable (pt,qTest,aSuccess,qClient,t,aDuration)
                createDirectoryIfMissing True $ path </> show pt
                TL.writeFile (path </> show pt </> show x ++ "-" ++ maybe "Prepare" (safely . fromTest) qTest <.> "txt") aStdout

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
