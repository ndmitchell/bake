{-# LANGUAGE RecordWildCards, TupleSections #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Brain(
    Memory(..), expire,
    Question(..), Answer(..), Ping(..),
    ClientInfo(..),
    prod
    ) where

import Development.Bake.Core.Run
import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Extra
import General.BigString
import Data.Tuple.Extra
import Data.Maybe
import Data.Monoid
import Control.Monad.Extra
import Data.List.Extra
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Bake.Server.Store
import Control.Exception.Extra
import General.HTML
import Prelude

import Development.Bake.Server.Memory
import Development.Bake.Server.Property


-- any question that has been asked of a client who hasn't pinged since the time is thrown away
expire :: UTCTime -> Memory -> Memory
expire cutoff s
    | null died = s
    | otherwise = s{running = filter (flip notElem died . qClient . snd) $ running s
                   ,clients = Map.map (\ci@ClientInfo{..} -> ci{ciAlive = ciAlive && pClient ciPing `notElem` died}) $ clients s}
    where died = [pClient ciPing | ClientInfo{..} <- Map.elems $ clients s, ciPingTime < cutoff, ciAlive]


prod :: Memory -> Message -> IO (Memory, Maybe (Either String Question))
prod mem msg = safely $ do
    res <- update mem msg
    case res of
        Left err -> return (mem, Just $ Left err)
        Right mem -> do
            mem <- reacts mem
            case msg of
                Pinged p | null $ fatal mem, Just q <- output (ovenTestInfo $ oven mem) mem p ->
                    case () of
                        -- we still test things on the skip list when testing on a state (to get some feedback)
                        _ | Just t <- qTest q, snd (qCandidate q) /= [], Just reason <- Map.lookup t (storeSkip $ store mem) -> do
                            prod mem $ Finished q $ Answer (bigStringFromString $ "Skipped due to being on the skip list\n" ++ reason) Nothing [] True
                        _ -> do
                            now <- getCurrentTime
                            return (mem{running = (now,q) : running mem}, Just $ Right q)
                _ -> return (mem, Nothing)
    where
        safely x = do
            res <- try_ x
            case res of
                Left e -> return (mem{fatal = show e : fatal mem}, Nothing)
                Right v -> return v


reacts :: Memory -> IO Memory
reacts = f 10
    where
        f 0 mem = return mem{fatal = "React got into a loop" : fatal mem}
        f i mem | null $ fatal mem, Just mem <- react mem = f (i-1) =<< mem
                | otherwise = return mem


failingTestOutput :: Store -> Point -> Maybe Test -> Maybe String
failingTestOutput store (state, patch) test = listToMaybe $ catMaybes
    [ storeRunFile store runid
    | (runid, _, _, Answer{aSuccess=False}) <- storeRunList store Nothing (Just test) (Just state) patch Nothing]


react :: Memory -> Maybe (IO Memory)
react mem@Memory{..}
    | xs <- rejectable mem
    , xs@(_:_) <- filter (\(p,t) -> t `Map.notMember` maybe Map.empty snd (paReject $ storePatch store p)) xs
    = Just $ do
        let fresh = filter (isNothing . paReject . storePatch store . fst) xs
        let point p = (fst active, takeWhile (/= p) (snd active) ++ [p])
        bad <- if fresh == [] then return id else do
            -- only notify on the first rejectable test for each patch
            Shower{..} <- shower mem False
            notify mem "Rejected"
                [ (paAuthor,) $ do
                    showPatch p <> str_ " submitted at " <> showTime paQueued
                    str_ " rejected due to " <> showTestAt (point p) t
                    whenJust (failingTestOutput store (point p) t) $ \s ->
                        br_ <> br_ <> pre_ (summary s)
                | (p,t) <- nubOrdOn fst xs, let PatchInfo{..} = storePatch store p]

        store <- storeUpdate store
            [IUReject p t (point p) | (p,t) <- xs]
        return $ bad mem{store = store}

    | plausible mem
    , xs@(_:_) <- filter (isNothing . paPlausible . storePatch store) $ snd active
    = Just $ do
        Shower{..} <- shower mem False
        -- don't notify people twice in quick succession
        bad <- if mergeable mem then return id else
            notify mem "Plausible"
                [ (paAuthor, showPatch p <> str_ " submitted at " <> showTime paQueued <> str_ " is now plausible")
                | p <- xs, let PatchInfo{..} = storePatch store p]
        store <- storeUpdate store $ map IUPlausible xs
        return $ bad mem{store = store}

    | mergeable mem
    , not $ null $ snd active
    = Just $ do
        (s, answer) <- if not simulated then uncurry runUpdate active else do
            s <- ovenUpdate oven (fst active) (snd active)
            return (Just s, Answer mempty (Just 0) mempty True)

        case s of
            Nothing -> do
                return mem{fatal = ("Failed to update\n" ++ bigStringToString (aStdout answer)) : fatal}
            Just s -> do
                Shower{..} <- shower mem False
                bad <- notify mem "Merged"
                    [ (paAuthor, showPatch p <> str_ " submitted at " <> showTime paQueued <> str_ " is now merged")
                    | p <- snd active, let PatchInfo{..} = storePatch store p]
                store <- storeUpdate store $ IUState s answer (Just active) : map IUMerge (snd active)
                return $ bad mem{active = (s, []), store = store}

    | restrictActive mem
    , (reject@(_:_), keep) <- partition (isJust . paReject . storePatch store) $ snd active
    = Just $ do
        return mem{active = (fst active, keep)}

    | not paused
    , extendActive mem
    , add@(_:_) <- Set.toList $ storeAlive store `Set.difference` Set.fromList (snd active)
    = Just $ do
        add <- sortOn (paQueued . storePatch store) add
        store <- storeUpdate store $ map IUStart add
        return mem
            {active = (fst active, snd active ++ add)
            ,store = store}

    | otherwise = Nothing


update :: Memory -> Message -> IO (Either String Memory)
update mem _ | fatal mem /= [] = return $ Right mem

update mem@Memory{..} (AddPatch author p) =
    if storeIsPatch store p then
        return $ Left "patch has already been submitted"
     else do
        let queued = storeAlive store `Set.difference` Set.fromList (snd active)
            supersede = filter (\old -> ovenSupersede oven old p) $ Set.toList queued
        store <- storeUpdate store $ IUQueue p author : map IUSupersede supersede
        return $ Right mem{store = store}

update mem@Memory{..} (DelPatch p) =
    if not $ p `Set.member` storeAlive store then
        return $ Left "patch is already dead or not known"
    else do
        store <- storeUpdate store [IUDelete p]
        return $ Right mem{store = store, active = second (delete p) active}

update mem@Memory{..} (SetState author s) = 
    if fst active == s then
        return $ Left "state is already at that value"
    else do
        store <- storeUpdate store [IUState s (Answer (bigStringFromString $ "From SetState by " ++ author) Nothing [] True) Nothing]
        return $ Right mem{store = store, active = (s, snd active)}

update mem@Memory{..} Requeue = do
    let add = Set.toList $ storeAlive store `Set.difference` Set.fromList (snd active)
    add <- sortOn (paQueued . storePatch store) add
    store <- storeUpdate store $ map IUStart add
    return $ Right mem
        {active = (fst active, snd active ++ add)
        ,store = store}

update mem@Memory{..} Pause
    | paused = return $ Left "already paused"
    | otherwise = return $ Right mem{paused = True}

update mem@Memory{..} Unpause
    | not paused = return $ Left "already unpaused"
    | otherwise = return $ Right mem{paused = False}

update mem@Memory{..} (Pinged ping) = do
    now <- getCurrentTime
    return $ Right mem{clients = Map.alter (Just . ClientInfo now ping True . maybe Map.empty ciTests) (pClient ping) clients}

update mem@Memory{..} (AddSkip author test)
    | test `Map.member` storeSkip store = return $ Left "already skipped"
    | otherwise = do
        store <- storeUpdate store [SUAdd test author]
        return $ Right mem{store = store}

update mem@Memory{..} (DelSkip test)
    | test `Map.notMember` storeSkip store = return $ Left "already not skipped"
    | otherwise = do
        store <- storeUpdate store [SUDel test]
        return $ Right mem{store = store}

update mem@Memory{..} (Finished q@Question{..} a@Answer{..}) = do
    bad <- case () of
        _ | snd qCandidate == [] -- on a state
          , not aSuccess
          , let skip = Set.mapMonotonic Just $ Map.keysSet $ storeSkip store
          , qTest `Set.notMember` skip -- not on the skip list
          , let failed = poFail $ storePoint store qCandidate
          , failed `Set.isSubsetOf` skip -- no notifications already
          -> do
            Shower{..} <- shower mem False
            notifyAdmins mem "State failure" $ do
                str_ "State " <> showState (fst qCandidate)
                str_ " failed due to " <> showTestAt qCandidate qTest <> br_ <> br_
                pre_ (bigStringWithString aStdout summary)
        _ -> return id

    now <- getCurrentTime
    let (eq,neq) = partition ((==) q . snd) running
    let time = head $ map fst eq ++ [now]
    store <- storeUpdate store [PURun time q a]
    let add ci = ci{ciTests = Map.insertWith (&&) (qCandidate, qTest) aSuccess $ ciTests ci}
    return $ Right $ bad mem
        {store = store
        ,clients = Map.adjust add qClient clients
        ,running = neq}


-- | Given a state, figure out what you should do next.
output :: (Test -> TestInfo Test) -> Memory -> Ping -> Maybe Question
output info mem Ping{..} | pNowThreads == 0 = Nothing
{-
1) try anyone who failed in active by bisecting
2) anyone not done in active or a superset
3) anyone not done in active
-}
output info mem@Memory{..} Ping{..} = -- trace (show (length bad, length good)) $
        fmap question $ enoughThreads $ listToMaybe $ filter suitable $ nubOrd $ concatMap dependencies $ bad ++ good
    where
        self = storePoint store active
        failedSelf = Set.toList $ poFail self
        failedPrefix = Map.fromListWith mappend $
            [ (t, case poTest po t of Just True -> ([i],[]); Just False -> ([],[i]); Nothing -> ([],[]))
            | (i, ps) <- zip [1..] $ tail $ inits $ snd active, let po = storePoint store (fst active, ps)
            , t <- failedSelf]

        bad = -- trace ("bisecting: " ++ show failedSelf) $
            [(i, t) | (t,(pass,fail)) <- Map.toList failedPrefix
                      -- assume 0 passed, so add to pass and delete from fail,
                      -- ensures we never try and "blame" 0 (which we can't reject)
                    , i <- bisect (0:pass) $ filter (/= 0) $ length (snd active):fail]

        setAddNothing = Set.insert Nothing . Set.mapMonotonic Just
        tests = setAddNothing $ fromMaybe Set.empty $ poTodo self
        doneSelf = poPass self `Set.union` poFail self
        passSuper = setAddNothing $ storeSupersetPass store active
        good = let (pri2,pri1) = partition (`Set.member` passSuper) $
                                 sortOn (maybe 0 $ negate . testPriority . info) $ Set.toList $
                                 tests `Set.difference` doneSelf
               in map (length $ snd active,) $ pri1 ++ pri2

        dependencies :: (Int, Maybe Test) -> [(Int, Maybe Test)]
        dependencies (i, t) = map (i,) $ flip transitiveClosure [t] $ \t -> case t of
            Nothing -> []
            Just t -> Nothing : map Just (testDepend $ info t)

        histDone = ciTests $ clients Map.! pClient
        histStarted = Map.keysSet histDone `Set.union` Set.fromList [(qCandidate, qTest) | (_,Question{..}) <- running, qClient == pClient]
        threadsForTest = fromMaybe pMaxThreads . testThreads . info

        -- if there are not enough threads, don't do anything else, just wait for threads to become available
        enoughThreads :: Maybe (Int, Maybe Test) -> Maybe (Int, Maybe Test)
        enoughThreads (Just (i, t)) | pNowThreads >= maybe 1 threadsForTest t = Just (i, t)
        enoughThreads _ = Nothing

        unprefix i = second (take i) active

        suitable :: (Int, Maybe Test) -> Bool
        suitable (i, Nothing)
            | (unprefix i,Nothing) `Set.notMember` histStarted -- I have not done it
            = True
        suitable (i,Just t)
            | (unprefix i,Just t) `Set.notMember` histStarted -- I have not done it
            , Map.lookup (unprefix i,Nothing) histDone == Just True -- I have prepared
            , Just ts <- poTodo $ storePoint store (unprefix i)
            , t `Set.member` ts -- this test is relevant to this patch
            , all (`elem` pProvide) $ testRequire $ info t -- I can do this test
            , all (\t -> Map.lookup (unprefix i, Just t) histDone == Just True) $ testDepend $ info t -- I have done all the dependencies
            = True
        suitable _ = False

        question (i, t) = Question (second (take i) active) t (maybe 1 threadsForTest t) pClient


-- | Given the passes, and the fails, suggest what you would like to try next
bisect :: [Int] -> [Int] -> [Int]
bisect pass fail
    | Just fail <- if null fail then Nothing else Just $ minimum fail
    , pass <- filter (< fail) pass
    , Just pass <- if null pass then Nothing else Just $ maximum pass
    = if fail - pass == 4 then [pass+2, pass+1, pass+3]
      else if fail - pass <= 3 then [pass+1 .. fail-1]
      else [(pass + fail) `div` 2]
bisect _ _ = []
