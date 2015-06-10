{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Test.Simulate(
    simulate
    ) where

import Development.Bake.Core.Message
import Development.Bake.Core.Type
import Development.Bake.Server.Brain
import Development.Bake.Server.Memory
import Development.Bake.Server.Store
import Control.Monad.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Data.Monoid
import Data.Maybe
import Numeric.Extra
import General.Extra
import System.Random
import System.IO.Extra
import System.Time.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude


simulate :: IO ()
simulate = withBuffering stdout NoBuffering $ do
    performance 200
    basic
    bisect
    newTest
    quickPlausible
    replicateM_ 20 randomSimple

---------------------------------------------------------------------
-- GENERIC SIMULATION ENGINE


-- Properties!
-- * By the time you finish, every patch must be propertly rejected or accepted
-- * Clients must not be executing too much at one time

data S s = S
    {user :: s
    ,memory :: Memory
    ,wait :: Int
    ,asked :: Set.Set Question
    ,patch :: [(Patch, Bool, Maybe Test -> Bool)]
    }

unstate :: State -> [Patch]
unstate = map toPatch . words . fromState

restate :: [Patch] -> State
restate = toState . unwords . map fromPatch

data Step = Submit Patch Bool (Maybe Test -> Bool) -- are you OK for it to pass, are you OK for it to fail
          | Reply Question Bool [Test]
          | Request Client
          | Paused Bool

simulation
    :: (Test -> TestInfo Test)                  -- ^ Static test information
    -> [(Client, Int)]                          -- ^ Clients, plus their maximum thread count
    -> s                                        -- ^ initial seed
    -> ([Question] -> s -> IO (s, Bool, Step))  -- ^ step function
    -> IO s
simulation testInfo workers u step = withTempDir $ \dir -> do
    t <- getCurrentTime
    s <- newStore True dir
    mem <- newMemory s (restate [], Answer mempty 0 [] True)
    mem <- return mem
        {active = (restate [], [])
        ,simulated = True}
    let s = S u mem 20 Set.empty []

    let count s c = sum [qThreads | (_, Question{..}) <- running $ memory s, qClient == c]
    let oven = defaultOven
            {ovenUpdate = \s ps -> return $ restate $ unstate s ++ ps
            ,ovenTestInfo = testInfo
            ,ovenSupersede = \_ _ -> False
            ,ovenInit = undefined
            ,ovenPrepare = undefined
            ,ovenPatchExtra = undefined
            }
    s@S{..} <- flip loopM s $ \s -> do
        -- print $ clients $ memory s
        -- print $ storePoint (store $ memory s) (active $ memory s)
        putChar '.'
        (u, cont, res) <- step (map snd $ running $ memory s) (user s)
        s <- return s{user = u}
        (msg,s) <- return $ case res of
            Submit p pass fail -> (AddPatch "" p, s{patch = (p,pass,fail) : patch s})
            Reply q good tests ->
                let ans = Answer mempty 0 (if good && isNothing (qTest q) then tests else []) good
                in (Finished q ans, s)
            Request c ->
                let Just mx = lookup c workers
                in (Pinged $ Ping c (fromClient c) [] mx $ mx - count s c, s)
            Paused b ->
                (if b then Pause "" else Unpause "", s)
        (mem, q) <- prod oven (memory s) msg
        q <- return $ either error id <$> q
        -- print q
        when (fatal mem /= []) $ error $ "Fatal error, " ++ unlines (fatal mem)
        s <- return s{memory = mem}
        s <- return $ case q of
            Just q | q `Set.member` asked s -> error "asking a duplicate question"
                   | otherwise -> s{asked = Set.insert q $ asked s}
            Nothing | not cont -> s{wait = wait s - 1}
            _ -> s
        return $ if wait s == 0 then Right s else Left s

    putStrLn ""
    let S{memory=Memory{..},..} = s

--    putStrLn $ unlines $ map show $ Map.toList $ storePoints store

    unless (null running) $ error "Running should have been empty"
    unless (null $ snd active) $ error "Active should have been empty"
    unless (Set.null $ storeAlive store) $ error "Alive should have been empty"
    forM_ workers $ \(c,_) -> do
        (_, q) <- prod oven Memory{..} $ Pinged $ Ping c (fromClient c) [] maxBound maxBound
        when (isJust q) $ error "Brains should have returned sleep"

    forM_ patch $ \(p, pass, fail) ->
        case () of
            _ | pass -> unless (p `elem` unstate (fst active)) $ error $ show ("expected pass but not",p)
              | PatchInfo{paReject=Just (_,t)} <- storePatch store p -> unless (all fail $ Map.keys t) $ error "incorrect test failure"
              | otherwise -> error "missing patch"
    return user


---------------------------------------------------------------------
-- SPECIFIC SIMULATIONS

randomSimple :: IO ()
randomSimple = do
    let info t = mempty{testDepend = [toTest "1" | t /= toTest "1"]}

    i <- randomRIO (0::Int,10)
    patches <- forM [0..i] $ \i -> do
        j <- randomRIO (0::Int,9)
        return $ toPatch $ show i ++ show j
    let failure t p = maybe "0" fromTest t `isSuffixOf` fromPatch p

    let client = toClient "c"
    simulation info [(client,2)] patches $ \active patches -> do
        i <- randomRIO (0::Int, 20)
        let cont = not $ null active && null patches
        case i of
            0 | p:patches <- patches -> do
                let pass = last (fromPatch p) > '3'
                return (patches, cont, Submit p pass (`failure` p))

            i | i <= 2, not $ null active -> do
                i <- randomRIO (0, length active - 1)
                let q = active !! i
                let good = not $ any (failure $ qTest q) $ unstate (fst $ qCandidate q) ++ snd (qCandidate q)
                return (patches, cont, Reply q good $ map toTest ["1","2","3"])

            _ -> return (patches, cont, Request client)
    putStrLn "Success at randomSimple"


quickPlausible :: IO ()
quickPlausible = do
    let info t = mempty{testPriority = if t == toTest "3" then 1 else if t == toTest "1" then -1 else 0}
    let client = toClient "c"
    let tests = map toTest ["1","2","3","4","5"]
    -- start, process 2 tests, add a patch, then process the rest
    -- expect to see 1, X, 1, rest, X

    let todo = [Paused True
               ,Submit (toPatch "0") True (const False)
               ,Submit (toPatch "1") False (== Just (toTest "3"))
               ,Submit (toPatch "2") True (const False)
               ,Paused False
               ,Submit (toPatch "3") True (const False)]

    simulation info [(client,1)] todo $ \active todo -> return $ case () of
        _ | t:odo <- todo -> (odo, True, t)
          | q@Question{..}:_ <- active ->
              if snd qCandidate == [toPatch "0",toPatch "2"] && qTest `notElem` [Nothing, Just $ toTest "3"] then error "bad quickPlausible"
              else (todo, True, Reply q (not $ qTest == Just (toTest "3") && toPatch "1" `elem` unstate (fst qCandidate) ++ snd qCandidate) tests)
          | otherwise -> (todo, False, Request client)
    putStrLn $ "Success at quickPlausible"


bisect :: IO ()
bisect = do
    let info t = mempty
    let tests = map (toTest . show) [1 .. 3 :: Int]
    let client = toClient "c"
    (done,_) <- simulation info [(client,1)] (0, map (toPatch . show) [1 .. 1024 :: Int]) $ \active (done,patches) -> return $ case () of
        _ | p:patches <- patches -> ((done,patches), True, Submit p (p /= toPatch "26") (\t -> p == toPatch "26" && t == Just (toTest "2")))
          | q:_ <- active -> ((done+1,[]), True, Reply q (qTest q /= Just (toTest "2") || toPatch "26" `notElem` (unstate (fst $ qCandidate q) ++ snd (qCandidate q))) tests)
          | otherwise -> ((done,[]), False, Request client)
    when (done > 50) $ error "Did too many tests to bisect"
    putStrLn "Success at bisect"


basic :: IO ()
basic = do
    -- have test x, fails in patch 4 of 5
    let info t = mempty
    let client = toClient "c"
    simulation info [(client,1)] (map (toPatch . show) [1..5 :: Int]) $ \active patches -> return $ case () of
        _ | p:patches <- patches -> (patches, True, Submit p (p /= toPatch "4") (\t -> p == toPatch "4" && t == Just (toTest "x")))
          | q:_ <- active, let isX = qTest q == Just (toTest "x"), let has12 = toPatch "4" `elem` snd (qCandidate q) ->
                ([], True, Reply q (not $ isX && has12) (map toTest ["x"]))
          | otherwise -> ([], False, Request client)
    putStrLn "Success at basic"


newTest :: IO ()
newTest = do
    -- had test x,y all along. Introduce z/q at newPatch, and z either always fails or always passes
    let info t = mempty
    let client = toClient "c"
    let tests = map toTest ["x","y"]
    let patches = map (toPatch . show) [1..20 :: Int]
    let newTest = toTest "z"
    let newPatch = toPatch "12"

    forM_ [False,True] $ \pass -> do
        simulation info [(client,1)] patches $ \active patches -> return $ case () of
            _ | p:patches <- patches -> (patches, True, Submit p (p /= newPatch || pass) (\t -> not pass && p == newPatch && t == Just newTest))
              | q:_ <- active, let isTest = qTest q == Just newTest, let isPatch = newPatch `elem` snd (qCandidate q) ->
                    if isTest && not isPatch then error $ "Running a test that doesn't exist, " ++ show q
                    else ([], True, Reply q (not isTest || pass) (tests ++ [newTest | isPatch] ++ [toTest "q" | isPatch]))
              | otherwise -> ([], False, Request client)
        putStrLn "Success at newtest"

performance :: Int -> IO ()
performance nTests = timed $ do
    -- TODO: ping the website regularly
    -- 1000 tests, 50 submissions, 7 failing, spawn about every 200 tests
    let nPatches = 50
    let f x = min (nTests-1) $ max 0 $ round $ intToDouble nTests * x 
    let fails = [(3,f 0.2),(4,f 0),(10,f 0.1),(22,f 0.6),(40,f 0.6),(48,f 0.9),(49,f 0.9)]

    let pri = toTest $ show $ f 0.1
    let npri = length $ fromTest pri
    let info t = mempty{testPriority = case compare (length $ fromTest t) npri of
                                            LT -> 1; GT -> 0; EQ -> if t < pri then 1 else 0}
    let client = toClient "c"
    let tests = map (toTest . show) [0 :: Int .. nTests - 1]
    simulation info [(client,3)] (0::Int, 0::Int) $ \active (patch,tick) -> return $ case () of
        _ | tick >= f 0.2, patch < nPatches ->
                let pass = patch `notElem` map fst fails
                    fail t = (patch,maybe (-1) (read . fromTest) t) `elem` fails
                in  ((patch+1, 0), True, Submit (toPatch $ show patch) pass fail)
          | q:_ <- active ->
                let pass = and [ (read $ fromPatch p, maybe (-1) (read . fromTest) $ qTest q) `notElem` fails
                               | p <- unstate (fst $ qCandidate q) ++ snd (qCandidate q)]
                in ((patch, tick+1), True, Reply q pass tests)
          | otherwise -> ((patch, tick), patch /= nPatches, Request client)
    putStrLn $ "Success at performance"
    where
        timed act = do
            (t, _) <- duration act
            putStrLn $ "Performance test took " ++ showDuration t
