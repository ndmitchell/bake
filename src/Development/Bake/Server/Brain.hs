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
import Data.Tuple.Extra
import Data.Maybe
import Data.Monoid
import General.Str
import Control.Monad
import Data.List.Extra
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Bake.Server.Store
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


prod :: Oven State Patch Test -> Memory -> Message -> IO (Memory, Maybe Question)
prod oven mem msg = do
    mem <- update oven mem msg
    mem <- reacts oven mem
    case msg of
        Pinged p | null $ fatal mem, Just q <- output (ovenTestInfo oven) mem p ->
            if maybe False (`Map.member` skipped mem) $ qTest q then
                prod oven mem $ Finished q $ Answer (strPack "Skipped due to being on the skip list") 0 [] True
            else do
                now <- getCurrentTime
                return (mem{running = (now,q) : running mem}, Just q)
        _ -> return (mem, Nothing)



reacts :: Oven State Patch Test -> Memory -> IO Memory
reacts oven = f 10
    where
        f 0 mem = return mem{fatal = "React got into a loop" : fatal mem}
        f i mem | null $ fatal mem, Just mem <- react oven mem = f (i-1) =<< mem
                | otherwise = return mem


react :: Oven State Patch Test -> Memory -> Maybe (IO Memory)
react oven mem@Memory{..}
    | xs <- rejectable mem
    , xs@(_:_) <- filter (\(p,t) -> t `Map.notMember` maybe Map.empty snd (paReject $ storePatch store p)) xs
    = Just $ do
        -- print $ "Rejecting: " ++ show xs
        -- print $ storePoint store (fst active, take 1 $ snd active)
        store <- storeUpdate store
            [IUReject p t (fst active, takeWhile (/= p) (snd active) ++ [p]) | (p,t) <- xs]
        return mem{store = store}

    | plausible mem
    , xs@(_:_) <- filter (isNothing . paPlausible . storePatch store) $ snd active
    = Just $ do
        store <- storeUpdate store $ map IUPlausible xs
        return mem{store = store}

    | mergeable mem
    , not $ null $ snd active
    = Just $ do
        (s, answer) <-
            if not simulated then uncurry runUpdate active
            else do s <- ovenUpdate oven (fst active) (snd active); return (Just s, Answer mempty 0 mempty True)

        let pauthors = map (snd . paQueued . storePatch store) $ snd active
        case s of
            Nothing -> do
                ovenNotify oven (authors ++ pauthors) "Failed to update, pretty serious"
                return mem
                    {fatal = ("Failed to update\n" ++ strUnpack (aStdout answer)) : fatal}
            Just s -> do
                ovenNotify oven authors "Your patch just made it in"
                storeSaveUpdate store s answer
                store <- storeUpdate store $ IUState s (Just active) : map IUMerge (snd active)
                return mem{active = (s, []), store = store}

    | restrictActive oven mem
    , (reject@(_:_), keep) <- partition (isJust . paReject . storePatch store) $ snd active
    = Just $ do
        let authors = map (snd . paQueued . storePatch store) reject
        ovenNotify oven authors "Rejected"
        return mem{active = (fst active, keep)}

    | extendActive mem
    , add@(_:_) <- Set.toList $ storeAlive store `Set.difference` Set.fromList (snd active)
    = Just $ do
        store <- storeUpdate store $ map IUStart add
        return mem
            {active = (fst active, snd active ++ sortOn (fst . paQueued . storePatch store) add)
            ,store = store}

    | otherwise = Nothing


update :: Oven State Patch Test -> Memory -> Message -> IO Memory
update oven mem@Memory{..} (AddPatch author p) =
    if storeIsPatch store p then
        error "patch has already been submitted"
     else do
        let queued = storeAlive store `Set.difference` Set.fromList (snd active)
            supersede = filter (\old -> ovenSupersede oven old p) $ Set.toList queued
        store <- storeUpdate store $ IUQueue p author : map IUSupersede supersede
        return mem{store = store}

update oven mem@Memory{..} (DelPatch _ p) =
    if not $ p `Set.member` storeAlive store then
        error "patch is already dead or not known"
    else do
        store <- storeUpdate store [IUDelete p]
        return mem{store = store, active = second (delete p) active}

update oven mem@Memory{..} (DelAllPatches author) = do
    store <- storeUpdate store $ map IUDelete $ Set.toList $ storeAlive store
    return mem{store = store, active = (fst active, [])}

update oven mem@Memory{..} (Requeue author) = do
    let add = Set.toList $ storeAlive store `Set.difference` Set.fromList (snd active)
    store <- storeUpdate store $ map IUStart add
    return mem
        {active = (fst active, snd active ++ sortOn (fst . paQueued . storePatch store) add)
        ,store = store}

update oven mem@Memory{..} (Pause _)
    | paused = error "already paused"
    | otherwise = return mem{paused = True}

update oven mem@Memory{..} (Unpause _)
    | not paused = error "already unpaused"
    | otherwise = return mem{paused = False}

update oven mem@Memory{..} (Pinged ping) = do
    now <- getCurrentTime
    return mem{clients = Map.alter (Just . ClientInfo now ping True . maybe Map.empty ciTests) (pClient ping) clients}

update oven mem@Memory{..} (AddSkip author test)
    | test `Map.member` skipped = error "already skipped"
    | otherwise = return mem{skipped = Map.insert test author skipped}

update oven mem@Memory{..} (DelSkip author test)
    | test `Map.notMember` skipped = error "already not skipped"
    | otherwise = return mem{skipped = Map.delete test skipped}

update oven mem@Memory{..} (Finished q@Question{..} a@Answer{..}) = do
    storeSaveTest store q a
    store <- storeUpdate store $
        [PUTest qCandidate aTests | aSuccess && qTest == Nothing] ++
        [(if aSuccess then PUPass else PUFail) qCandidate qTest]
    let add ci = ci{ciTests = Map.insertWith (&&) (qCandidate, qTest) aSuccess $ ciTests ci}
    return mem{store = store
              ,clients = Map.adjust add qClient clients
              ,running = filter ((/=) q . snd) running}

update oven mem (Reinit s) = return mem -- already handled


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
