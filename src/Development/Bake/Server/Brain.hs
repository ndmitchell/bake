{-# LANGUAGE RecordWildCards, TupleSections #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Brain(
    Memory(..), stateFailure, new, expire,
    Question(..), Answer(..), Ping(..),
    PingEx(..), Update(..), rebase,
    prod
    ) where

import Control.DeepSeq
import Development.Bake.Core.Run
import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Extra
import Data.Tuple.Extra
import Data.Maybe
import Data.Monoid
-- import Debug.Trace
import Control.Monad
import Control.Exception.Extra
import Data.List.Extra
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Prelude

trace _ x = x

---------------------------------------------------------------------
-- THE DATA TYPE

data PingEx = PingEx
    {piTime :: UTCTime
    ,piPing :: Ping
    ,piAlive :: Bool
    } deriving (Eq,Show)

data Update = Update
    {upTime :: UTCTime
    ,upAnswer :: Answer
    ,upState :: State
    ,upPrevious :: [Patch] -- those which were applied to the previous state
    } deriving (Eq,Show)

data Memory = Memory
    {patches :: [(UTCTime, Patch)]
        -- ^ List of all patches that have been submitted over time
    ,updates :: [Update]
        -- ^ Updates that have been made. If the Answer failed, you must have an entry in fatal
    ,authors :: Map (Maybe Patch) [Author]
        -- ^ Authors associated with each patch (Nothing is the server author)
    ,fatal :: [String]
        -- ^ A list of fatal error messages that have been raised by the server
    ,simulated :: Bool
        -- ^ Are we running in a simulation (don't spawn separate process)

    ,pings :: Map Client PingEx
        -- ^ Latest time of a ping sent by each client
    ,history :: [(UTCTime, Question, Answer)]
        -- ^ Questions you have sent to clients, and how they responded.
    ,running :: [(UTCTime, Question)]
        -- ^ Questions you have sent to clients and are waiting for.

    ,rejected :: Map Patch (Set (Maybe Test))
        -- ^ Reasons a particular patch was rejected

    ,paused :: Bool
        -- ^ Pretend the queued is empty
    ,queued :: [Patch]
        -- ^ People who are queued up
    ,active :: (State, [Patch])
        -- ^ the target we are working at (some may already be rejected)
    } deriving Show

instance NFData Memory where
    rnf Memory{} = () -- I don't ever intend putting something lazy in Memory


stateFailure = State ""

-- | Create a new piece of memory.
new :: Memory
new = Memory [] [] Map.empty [] False Map.empty [] [] Map.empty False [] (stateFailure, [])


-- any question that has been asked of a client who hasn't pinged since the time is thrown away
expire :: UTCTime -> Memory -> Memory
expire cutoff s
    | null died = s
    | otherwise = s{running = filter (flip notElem died . qClient . snd) $ running s
                   ,pings = Map.map (\pi@PingEx{..} -> pi{piAlive = piAlive && pClient piPing `notElem` died}) $ pings s}
    where died = [pClient piPing | PingEx{..} <- Map.elems $ pings s, piTime < cutoff, piAlive]


consistent :: Memory -> IO ()
consistent mem@Memory{..} = do
    -- basic sanity checks
    when (fst active /= upState (head updates)) $ error "active is not working of the most recent update"

    -- all preparations for a given point are equal
    let xs = groupSort $ map (qCandidate . snd3 &&& id) $ filter (isNothing . qTest . snd3) history
    forM_ xs $ \(c,vs) -> do
        case nubOrd $ map (sort . aTests) $ filter aSuccess $ map thd3 vs of
            a:b:_ -> error $ "serverConsistent: Tests don't match for candidate: " ++ show (c,a,b,vs)
            _ -> return ()


data Rebase = Active -- exactly equal to 'active' (s,ps)
            | Prefix Int -- a prefix of the current 'active' (s,take i ps)
            | Superset -- a superset of 'active', (s,?p?s?) - all patches in order, plus others
              deriving (Eq,Show)

-- Active gets mapped to Prefix, Superset becomes Nothing
rebasePrefix :: Memory -> ((State, [Patch]) -> Maybe Int)
rebasePrefix mem = \x -> case rb x of
        Just Active -> Just n
        Just (Prefix i) -> Just i
        _ -> Nothing
    where
        n = length $ snd $ active mem
        rb = rebase mem

rebase :: Memory -> ((State, [Patch]) -> Maybe Rebase)
rebase Memory{..} = \(s,ps) -> case Map.lookup s mp of
    Just pre
        | Just ps <- stripPrefix pre ps, ps `isPrefixOf` snd active
            -> Just $ let i = length ps in if i == n then Active else Prefix i
        | isSuperset (pre ++ ps) (snd active) -> Just Superset
    _ -> Nothing
    where
        n = length $ snd active
        -- mapping from state to exact list of patches to get to the front of the list
        mp = Map.fromList $ zip (map upState updates) (map (concat . reverse) $ inits $ map upPrevious updates)

        isSuperset (x:xs) (y:ys) = if x == y then isSuperset xs ys else isSuperset xs (y:ys)
        isSuperset _ ys = null ys


prod :: Oven State Patch Test -> Memory -> Message -> IO (Memory, Maybe Question)
prod oven mem msg = do
    mem <- input oven mem msg
    now <- getCurrentTime
    return $ case msg of
        Pinged p | Just q <- output (ovenTestInfo oven) mem p -> (mem{running = (now,q) : running mem}, Just q)
        _ -> (mem, Nothing)



-- | Given a state, produce more.
input :: Oven State Patch Test -> Memory -> Message -> IO Memory
input oven mem msg | fatal mem /= [] = return mem
input oven mem msg = do
    now <- getCurrentTime
    mem <- return $ reject $ reinput oven now mem msg
    let f mem | fatal mem == [], Just mem <- reactive oven mem = f . reject =<< mem
              | otherwise = return mem
    mem <- f mem
    res <- try_ $ when (fatal mem == []) $ consistent mem
    case res of
        Right () -> return mem
        Left e -> do
            e <- showException e
            return mem{fatal = ("Consistency check failed: " ++ e) : fatal mem}


reject :: Memory -> Memory
-- find tests which have a passed/failed one apart in Prefix,
-- assume 0 is implicitly passing
-- and anywhere the test isn't available
reject mem@Memory{..} = foldl' use mem $ concatMap bad results
    where
        rbPrefix = rebasePrefix mem

        -- [(test, ([pass], [fail]))]
        results = Map.toList $ Map.fromListWith mappend
            [ (qTest, if aSuccess then ([i],[]) else ([],[i]))
            | (_,Question{..},Answer{..}) <- history
            , Just i <- [rbPrefix qCandidate]]

        -- Map prefix (Set test)
        prepare = Map.fromList
            [ (i, Set.fromList $ aTests a)
            | (_,Question{..},a@Answer{..}) <- history
            , aSuccess, qTest == Nothing
            , Just i <- [rbPrefix qCandidate]]

        -- 0: makes the assumption the base state passes all tests
        bad :: (Maybe Test, ([Int], [Int])) -> [(Patch, Maybe Test)]
        bad (t, (pass, fail)) = [(snd active !! (i-1), t) | i <- fail,
            (i-1) `elem` (0:pass) ||
            Just False == (do t <- t; ts <- Map.lookup (i-1) prepare; return $ t `Set.member` ts)]

        use mem@Memory{..} (p, t) = mem{rejected = Map.insertWith Set.union p (Set.singleton t) rejected}


reactive :: Oven State Patch Test -> Memory -> Maybe (IO Memory)
reactive oven mem@Memory{..}
    -- if active has passed all tests and none of the patches are on rejected
    | snd active /= []
    , reject == []
    , tests == Just (passed self) = Just $ do
        now <- getCurrentTime
        (s, answer) <-
            if not simulated then uncurry runUpdate active
            else do s <- ovenUpdateState oven $ Just active; return (Just s, Answer mempty 0 mempty True)

        case s of
            Nothing -> do
                ovenNotify oven [a | p <- Nothing : map Just (snd active), a <- Map.findWithDefault [] p authors]
                    "Failed to update, pretty serious"
                return mem
                    {fatal = "Failed to update" : fatal
                    ,updates = Update now answer stateFailure (snd active) : updates}
            Just s -> do
                ovenNotify oven [a | p <- snd active, a <- Map.findWithDefault [] (Just p) authors]
                    "Your patch just made it in"
                return mem
                    {updates = Update now answer s (snd active) : updates
                    ,active = (s, [])
                    }

    -- if active or a superset passed all tests and none are rejected and not paused
    | not paused
    , queued /= []
    , reject == []
    , snd active == [] || tests == Just (passed $ self ++ superset) = 
        -- requeue
        Just $ return mem{active = second (++ queued) active, queued = []}

    -- if all tests either (passed on active or a superset)
    --              or     (failed on active and lead to a rejection)
    --              or     (depend on a test that failed)
    | reject /= []
    , Just tests <- tests
    , let tPass = passed $ self ++ superset
    , let tFail = Set.fromList $ catMaybes $ concatMap (Set.toList . snd) reject
    , flip all (Set.toList tests) $ \t ->
        t `Set.member` tPass || any (`Set.member` tFail) (transitiveClosure (testRequire . ovenTestInfo oven) [t]) =
        -- exclude the rejected from active
        Just $ return mem{active = second (\\ map fst reject) active}

    -- preparing failed and I can reject someone for preparation
    | reject /= []
    , not $ null [() | (_,q,a) <- self, qTest q == Nothing, not $ aSuccess a]
    , any (Set.member Nothing . snd) reject =
        -- exclude the rejected from active
        Just $ return mem{active = second (\\ map fst reject) active}

    | otherwise = Nothing -- trace (show ("reactive", length reject, show tests, show self)) $ Nothing
    where
        rb = rebase mem
        self     = filter ((== Just Active  ) . rb . qCandidate . snd3) history
        superset = filter ((== Just Superset) . rb . qCandidate . snd3) history
        passed tqa = Set.fromList [t | (_,Question{qTest=Just t},Answer{aSuccess=True}) <- tqa]
        tests = listToMaybe [Set.fromList $ aTests a | (_,q,a) <- self, qTest q == Nothing, aSuccess a]
        reject = [(p, ts) | p <- snd active, Just ts <- [Map.lookup p rejected]]


reinput :: Oven State Patch Test -> UTCTime -> Memory -> Message -> Memory
reinput oven now mem@Memory{..} (AddPatch author p) =
    if p `elem` map snd patches then
        error "patch has already been submitted"
     else mem
        {queued = filter (\old -> not $ old == p || ovenSupersede oven old p) queued `snoc` p
        ,authors = Map.insertWith (++) (Just p) [author] authors
        ,patches = (now,p) : patches}

reinput oven now mem@Memory{..} (DelPatch _ p) = mem
    {queued = filter (/= p) queued
    ,active = second (delete p) active}

reinput oven now mem@Memory{..} (DelAllPatches _) = mem
    {queued = []
    ,active = (fst active, [])}

reinput oven now mem@Memory{..} (Requeue _) = mem
    {queued = []
    ,active = second (++ queued) active}

reinput oven now mem@Memory{..} (Pause _)
    | paused = error "already paused"
    | otherwise = mem{paused = True}

reinput oven now mem@Memory{..} (Unpause _)
    | not paused = error "already unpaused"
    | otherwise = mem{paused = False}

reinput oven now mem@Memory{..} (Pinged ping) =
    mem{pings = Map.insert (pClient ping) (PingEx now ping True) pings}

reinput oven now mem (Finished q a) =
    mem{running = other, history = (head $ map fst this `snoc` now, q, a) : history mem}
    where (this,other) = partition ((==) q . snd) $ running mem


-- | Given a state, figure out what you should do next.
output :: (Test -> TestInfo Test) -> Memory -> Ping -> Maybe Question
{-
1) try anyone who failed in active by bisecting
2) anyone not done in active or a superset
3) anyone not done in active
-}
output info mem@Memory{..} Ping{..} = trace (show (length bad, length good)) $ listToMaybe $ map question $ filter suitable $ nubOrd $ concatMap dependencies $ bad ++ good
    where
        rb = rebase mem
        rbPrefix = rebasePrefix mem
        self = [(q, a) | (_,q,a) <- history, rb (qCandidate q) == Just Active]

        failedSelf = Set.fromList [qTest q | (q, a) <- self, not $ aSuccess a]
        failedPrefix = Map.fromListWith mappend $
                [(qTest q, if aSuccess a then ([i],[]) else ([],[i]))
                    | (_,q,a) <- history, qTest q `Set.member` failedSelf, Just (Prefix i) <- [rb (qCandidate q)]] ++
                map (,mempty) (Set.toList failedSelf)
        bad = trace ("bisecting: " ++ show failedSelf) $
            [(i, t) | (t,(pass,fail)) <- Map.toList failedPrefix
                      -- assume 0 passed, so add to pass and delete from fail,
                      -- ensures we never try and "blame" 0 (which we can't reject)
                    , i <- bisect (0:pass) $ filter (/= 0) $ length (snd active):fail]

        tests = Set.fromList $ Nothing : concat (take 1 [map Just $ aTests a | (q, a) <- self, qTest q == Nothing, aSuccess a])
        doneSelf = Set.fromList [qTest q | (q, a) <- self]
        passSuper = Set.fromList [qTest q | (_,q,a) <- history, rb (qCandidate q) == Just Superset, aSuccess a]
        good = let (pri2,pri1) = partition (`Set.member` passSuper) $
                                 sortOn (maybe 0 $ negate . testPriority . info) $ Set.toList $
                                 tests `Set.difference` doneSelf
               in map (length $ snd active,) $ pri1 ++ pri2

        dependencies :: (Int, Maybe Test) -> [(Int, Maybe Test)]
        dependencies (i, t) = map (i,) $ flip transitiveClosure [t] $ \t -> case t of
            Nothing -> []
            Just t -> Nothing : map Just (testRequire $ info t)

        hist = Map.fromListWith (++) [((i,qTest q),[a])
            | (q,a) <- map (snd3 &&& Just . thd3) history ++ map ((,Nothing) . snd) running
            , qClient q == pClient, Just i <- [rbPrefix $ qCandidate q]]
        threadsForTest = fromMaybe pMaxThreads . testThreads . info

        suitable :: (Int, Maybe Test) -> Bool
        suitable (i, Nothing)
            | pNowThreads >= 1 -- enough threads
            , (i,Nothing) `Map.notMember` hist -- I have not done it
            = True
        suitable (i,Just t)
            | pNowThreads >= threadsForTest t -- enough threads
            , (i,Just t) `Map.notMember` hist -- I have not done it
            , (poss,_):_ <- map aTestsSuitable $ filter aSuccess $ catMaybes $ Map.findWithDefault [] (i,Nothing) hist
            , t `elem` poss -- it is one of the test this client can do
            , all (\t -> any (maybe False aSuccess) $ Map.findWithDefault [] (i,Just t) hist) $ testRequire $ info t -- I have done all the dependencies
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
