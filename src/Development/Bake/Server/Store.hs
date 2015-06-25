{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections, TypeOperators, ViewPatterns #-}

-- Stuff on disk on the server
module Development.Bake.Server.Store(
    Store, newStore, storeSave, storeSQL,
    PatchInfo(..), paAlive, storeIsPatch, storePatch, storeAlive,
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
import qualified Data.Set as Set
import qualified Data.Map as Map
import General.Extra
import System.Time.Extra
import Data.Char
import Data.List.Extra
import System.IO.Unsafe
import Data.Monoid
import Data.Maybe
import Control.Concurrent.Extra
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

---------------------------------------------------------------------
-- DATA TYPES

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
    {paAuthor :: Author
    ,paQueued :: UTCTime
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
    ,stDuration :: Maybe Seconds
    }


---------------------------------------------------------------------
-- CACHED INFORMATION

data Cache = Cache
    {cachePointId :: PointId -> IO Point
    ,cachePatch :: Patch -> IO (PatchId, PatchInfo)
    ,cacheState :: State -> IO (StateId, StateInfo)
    ,cachePoint :: Point -> IO (PointId, PointInfo)
    ,cacheSkip :: IO (Map.Map Test String)
    ,cacheAlive :: IO (Set.Set Patch)
    ,cacheSupersetPass :: Point -> IO (Set.Set Test)
    }

newCache :: Connection -> IO Cache
newCache conn = do
    cachePointId <- memoIO1 $ \pt -> do
        [(s,ps)] <- sqlSelect conn (ptState, ptPatches) [ptId %== pt]
        [Only s] <- sqlSelect conn (Only saState) [saId %== s]
        ps <- forM (fromPatchIds ps) $ \p -> do
            [Only p] <- sqlSelect conn pcPatch [pcId %== p]; return p
        return (s, ps)

    cachePatch <- memoIO1 $ \p -> do
        [(row, paAuthor, paQueue, paStart, paDelete, paSupersede, paReject, paPlausible, paMerge)] <-
            sqlSelect conn (pcId,pcAuthor,pcQueue,pcStart,pcDelete,pcSupersede,pcReject,pcPlausible,pcMerge)
            [pcPatch %== p]
        reject <- if isNothing paReject then return Nothing else unsafeInterleaveIO $ do
            ts <- sqlSelect conn (rjTest, rnPoint) [distinct rjTest, rjPatch %== row, rjRun %==% rnId]
            ts <- mapM (\(a,b) -> (a,) <$> cachePointId b) ts
            return (Just (fromJust paReject, Map.fromList ts))
        return (row, PatchInfo paAuthor paQueue paStart paDelete paSupersede reject paPlausible paMerge)

    cacheState <- memoIO1 $ \s -> do
        let checkOne msg [x] = [x]
            checkOne msg xs = error $ "checkOne, expected 1 but got " ++ show (length xs) ++ ", " ++ msg
        [(row, sCreate, sPoint, sDuration)] <- checkOne ("Loading up state " ++ show s) <$> sqlSelect conn (saId, saCreate, saPoint, saDuration) [saState %== s]
        pt <- maybe (return Nothing) (fmap Just . cachePointId) sPoint
        return (row, StateInfo sCreate pt sDuration)

    cachePoint <- memoIO1 $ \(s,ps) -> do
        s <- fst <$> cacheState s
        ps <- patchIds <$> mapM (fmap fst . cachePatch) ps
        res <- sqlSelect conn ptId [ptState %== s, ptPatches %== ps]
        pt <- case res of
            [] -> sqlInsert conn ptTable (s, ps)
            [Only x] -> return x
            _ -> error $ "ensurePoint, multiple points found"
        tests <- unsafeInterleaveIO $ sqlSelect conn tsTest [tsPoint %== pt]
        pass <- unsafeInterleaveIO $ sqlSelect conn rnTest [distinct rnTest, rnPoint %== pt, rnSuccess %== True]
        fail <- unsafeInterleaveIO $ sqlSelect conn rnTest [distinct rnTest, rnPoint %== pt, rnSuccess %== False]
        return $ (,) pt $ PointInfo
            (if null tests then Nothing else Just $ Set.fromList $ mapMaybe fromOnly tests)
            (Set.fromList $ map fromOnly pass) (Set.fromList $ map fromOnly fail)

    cacheSkip <- memoIO0 $ do
        Map.fromList <$> sqlSelect conn (skTest, skComment) []

    cacheAlive <- memoIO0 $ do
        ps <- sqlSelect conn pcPatch [nullP pcDelete, nullP pcSupersede, nullP pcReject, nullP pcMerge]
        return $ Set.fromList $ map fromOnly ps

    cacheSupersetPass <- memoIO1 $ \(s, ps) -> do
        s <- fst <$> cacheState s
        ps <- mapM (fmap fst . cachePatch) ps
        let f success = do
                xs <- sqlSelect conn rnTest [rnPoint %==% ptId, ptState %== s, likeP ptPatches $ patchIdsSuperset ps, rnSuccess %== success]
                return $ Set.fromList $ mapMaybe fromOnly xs
        liftM2 Set.difference (f True) (f False)

    return Cache{..}


---------------------------------------------------------------------
-- STORED DATA

data Store = Store
    {conn :: Connection
    ,path :: FilePath
    ,cache :: Cache
    ,extra :: Var (Map.Map (Either State Patch) (Maybe T.Text))
    }

instance Show Store where
    show Store{..} = show path

newStore :: Bool -> FilePath -> IO Store
newStore mem path = do
    createDirectoryIfMissing True path
    conn <- create $ if mem then Nothing else Just $ path </> "bake.sqlite"
    cache <- newCache conn
    extra <- newVar Map.empty
    return $ Store conn path cache extra

storeSave :: FilePath -> Store -> IO ()
storeSave file Store{..} = do
    whenM (doesFileExist file) $ removeFile file
    save conn file

storeSQL :: (ToRow q, FromRow r) => Store -> String -> q -> IO [r]
storeSQL Store{..} = sqlUnsafe conn


---------------------------------------------------------------------
-- QUERIES

storePoint :: Store -> Point -> PointInfo
storePoint Store{..} = snd . unsafePerformIO . cachePoint cache

storeIsPatch :: Store -> Patch -> Bool
storeIsPatch Store{..} p = unsafePerformIO $ do
    ps <- sqlSelect conn pcPatch [pcPatch %== p]
    return $ ps /= []

storePatch :: Store -> Patch -> PatchInfo
storePatch Store{..} = snd . unsafePerformIO . cachePatch cache

storeState :: Store -> State -> StateInfo
storeState Store{..} = snd . unsafePerformIO . cacheState cache

data PP = PP {ppPatch :: Patch, ppReject :: Bool, ppMx :: UTCTime}
instance FromRow PP where fromRow = PP <$> field <*> field <*> field

storeItemsDate :: Store -> (UTCTime, Maybe UTCTime) -> [Either State Patch]
storeItemsDate Store{..} (start, end) = unsafePerformIO $ do
    let ends = words "start delete_ supersede reject plausible merge"
    let str = "SELECT patch, reject IS NOT NULL, max(" ++ intercalate "," ["ifnull(" ++ x ++ ",queue)" | x <- ends] ++ ") AS mx " ++
              "FROM patch WHERE mx > ?" ++
                    (if isJust end then " AND queue < ?"
                     else " OR (delete_ IS NULL AND supersede IS NULL AND reject IS NULL AND merge IS NULL)") ++
              " ORDER BY queue ASC"
    patches :: [PP] <- sqlUnsafe conn str $ start : maybeToList end

    states <- sqlSelect conn (saState, saCreate) $ [orderAsc saCreate, saState %/= toState "", saCreate %> start] ++ [saCreate %< end | Just end <- [end]]
    return $ reverse $ merge states patches
    where
        merge (s:ss) o@(span ppReject -> (reject, p:ps))
            | snd s < ppMx p = Left (fst s) : merge ss o
            | otherwise = map (Right . ppPatch) (reject ++ [p]) ++ merge (s:ss) ps
        merge ss ps = map (Left . fst) ss ++ map (Right . ppPatch) ps


storeSkip :: Store -> Map.Map Test String
storeSkip Store{..} = unsafePerformIO $ cacheSkip cache

storeAlive :: Store -> Set.Set Patch
storeAlive Store{..} = unsafePerformIO $ cacheAlive cache

storeSupersetPass :: Store -> (State,[Patch]) -> Set.Set Test
storeSupersetPass Store{..} = unsafePerformIO . cacheSupersetPass cache


storeRunList :: Store -> Maybe Client -> Maybe (Maybe Test) -> Maybe State -> [Patch] -> Maybe RunId -> [(RunId, UTCTime, Question, Answer)]
storeRunList Store{..} client test state patches run = unsafePerformIO $ do
    point <- maybe (return Nothing) (fmap (Just . fst) . cachePoint cache . (, patches)) state
    patches <- if isNothing state && patches /= [] then Just <$> mapM (fmap fst . cachePatch cache) patches else return Nothing
    let filt = [rnClient %== x | Just x <- [client]] ++
               [rnTest %== x | Just x <- [test]] ++
               [ptId %==% rnPoint %&& likeP ptPatches (patchIdsSuperset x) | Just x <- [patches]] ++
               [rnPoint %== x | Just x <- [point]] ++
               [rnId %== x | Just x <- [run]]
    xs <- sqlSelect conn (rnId, rnPoint, rnTest, rnSuccess, rnClient, rnStart, rnDuration) (orderDesc rnStart : limit 1001 : filt)
    forM xs $ \(rnId, rnPoint, rnTest, rnSuccess, rnClient, rnStart, rnDuration) -> do
        pt <- cachePointId cache rnPoint
        return (rnId, rnStart, Question pt rnTest 0 rnClient, Answer mempty rnDuration [] rnSuccess)

storeStateList :: Store -> [(State, StateInfo)]
storeStateList Store{..} = unsafePerformIO $ do
    xs <- sqlSelect conn (saState, saCreate, saPoint, saDuration) [orderDesc saCreate, limit 1000]
    forM xs $ \(sState, sCreate, sPoint, sDuration) -> do
        pt <- maybe (return Nothing) (fmap Just . cachePointId cache) sPoint
        return (sState, StateInfo sCreate pt sDuration)


---------------------------------------------------------------------
-- UPDATES

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

-- don't inline so GHC can't tell the store is returned unchanged
{-# NOINLINE storeUpdate #-}
storeUpdate :: Store -> [Update] -> IO Store
storeUpdate store xs = do
    -- important so that if the updates depend on the current store they are forced first
    -- the perils of impurity!
    evaluate $ rnf $ show xs

    now <- getCurrentTime
    (\f -> foldM f store xs) $ \store x -> do
        f now store x
        cache <- newCache $ conn store
        return store{cache=cache}
    where
        f now Store{..} x = case x of
            IUState s Answer{..} p -> do
                pt <- maybe (return Nothing) (fmap (Just . fst) . cachePoint cache) p
                prev <- sqlSelect conn saId [saState %== s]
                x <- case prev of
                    [] ->
                        sqlInsert conn saTable (s,now,pt,aDuration)
                    Only x:_ -> do
                        sqlUpdate conn [saCreate := now, saPoint := pt, saDuration := aDuration] [saId %== x]
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
                pt2 <- fst <$> cachePoint cache pt
                pa <- fst <$> cachePatch cache p
                Only run:_ <- sqlSelect conn rnId [rnSuccess %== False, rnPoint %== pt2, rnTest %== t]
                sqlUpdate conn [pcReject := Just now] [pcPatch %== p]
                void $ sqlInsert conn rjTable (pa, t, run)
            SUAdd t msg -> do
                void $ sqlInsert conn skTable (t, msg)
            SUDel t -> do
                sqlDelete conn skTable [skTest %== t]
            PURun t Question{..} Answer{..} -> do
                pt <- fst <$> cachePoint cache qCandidate
                when (qTest == Nothing) $ do
                    res :: [Only (Maybe Test)] <- sqlSelect conn tsTest [tsPoint %== pt]
                    if null res then do
                        sqlInsert conn tsTable (pt, Nothing)
                        forM_ aTests $ \t -> sqlInsert conn tsTable (pt, Just t)
                    else
                        when (Set.fromList (mapMaybe fromOnly res) /= Set.fromList aTests) $
                            putStrLn $ "Warning: Test disagreement at " ++ show pt ++ ", maybe a changed generator?"
                x <- sqlInsert conn rnTable (pt,qTest,aSuccess,qClient,t,aDuration)
                createDirectoryIfMissing True $ path </> show pt
                TL.writeFile (path </> show pt </> show x ++ "-" ++ maybe "Prepare" (safely . fromTest) qTest <.> "txt") aStdout

safely :: String -> String
safely = map f . take 100
    where f x | isAlphaNum x || x `elem` (".-_" :: String) = x
          f x = '_'


storeStateFile :: Store -> State -> Maybe String
storeStateFile Store{..} st = unsafePerformIO $ do
    st <- fst <$> cacheState cache st
    let file = path </> show st </> "update.txt"
    ifM (doesFileExist file) (Just <$> readFile file) (return Nothing)


storeRunFile :: Store -> RunId -> Maybe String
storeRunFile Store{..} run = unsafePerformIO $ do
    [(rPoint, rTest)] <- sqlSelect conn (rnPoint, rnTest) [rnId %== run]
    let file = path </> show rPoint </> show run ++ "-" ++ maybe "Prepare" (safely . fromTest) rTest <.> "txt"
    ifM (doesFileExist file) (Just <$> readFile file) (return Nothing)


storeExtraFile :: Store -> Either State Patch -> IO FilePath
storeExtraFile Store{..} x = (path </>) <$> either (fmap (show . fst) . cacheState cache) (fmap (show . fst) . cachePatch cache) x


storeExtra :: Store -> Either State Patch -> Maybe (String, String)
storeExtra store@Store{..} sp = unsafePerformIO $ do
    prefix <- storeExtraFile store sp
    short <- modifyVar extra $ \mp ->
        case Map.lookup sp mp of
            Just v -> return (mp, v)
            Nothing -> do
                short <- ifM (doesFileExist $ prefix </> "extra-short.html") (fmap Just $ T.readFile $ prefix </> "extra-short.html") (return Nothing)
                return (Map.insert sp short mp, short)
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
    modifyVar_ extra $ return . Map.insert sp (Just short)
