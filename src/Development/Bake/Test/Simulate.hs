{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Test.Simulate(
    simulate
    ) where

import Development.Bake.Core.Message
import Development.Bake.Core.Type
import Development.Bake.Server.Brain
import Control.Monad.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Data.Monoid
import Data.Maybe
import Numeric.Extra
import General.Str
import General.Extra
import System.Random
import System.IO.Extra
import System.Time.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude


simulate :: IO ()
simulate = withBuffering stdout NoBuffering $ do
    when False $ do
        (t,_) <- duration $ performance 200
        putStrLn $ "Performance test took " ++ showDuration t
    bisect
    when False quickPlausible
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
unstate = map Patch . words . fromState

restate :: [Patch] -> State
restate = State . unwords . map fromPatch

data Step = Submit Patch Bool (Maybe Test -> Bool) -- are you OK for it to pass, are you OK for it to fail
          | Reply Question Bool ([Test], [Test])
          | Request Client

simulation
    :: (Test -> TestInfo Test)                  -- ^ Static test information
    -> [(Client, Int)]                          -- ^ Clients, plus their maximum thread count
    -> s                                        -- ^ initial seed
    -> ([Question] -> s -> IO (s, Bool, Step))  -- ^ step function
    -> IO s
simulation testInfo clients u step = do
    t <- getCurrentTime
    let mem = new
            {active = (restate [], [])
            ,simulated = True
            ,updates = [Update t (Answer (strPack "") 0 mempty True) (restate []) []]}
    let s = S u mem 20 Set.empty []

    let count s c = sum [qThreads | (_, Question{..}) <- running $ memory s, qClient == c]
    let oven = defaultOven
            {ovenUpdateState = \(Just (s, ps)) -> return $ restate $ unstate s ++ ps
            ,ovenPrepare = undefined
            ,ovenTestInfo = testInfo
            ,ovenPatchExtra = undefined
            ,ovenStringyState = undefined
            ,ovenStringyPatch = undefined
            ,ovenStringyTest = undefined
            }
    s@S{..} <- flip loopM s $ \s -> do
        putChar '.'
        (u, cont, res) <- step (map snd $ running $ memory s) (user s)
        s <- return s{user = u}
        (msg,s) <- return $ case res of
            Submit p pass fail -> (AddPatch "" p, s{patch = (p,pass,fail) : patch s})
            Reply q good tests ->
                let ans = Answer (strPack "") 0 (if good && isNothing (qTest q) then tests else mempty) good
                in (Finished q ans, s)
            Request c ->
                let Just mx = lookup c clients
                in (Pinged $ Ping c (fromClient c) mx $ mx - count s c, s)
        (mem, q) <- prod oven (memory s) msg
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

    unless (null running) $ error "Active should have been empty"
    forM_ clients $ \(c,_) -> do
        (_, q) <- prod oven Memory{..} $ Pinged $ Ping c (fromClient c) maxBound maxBound
        when (isJust q) $ error "Brains should have returned sleep"
    when (snd active /= []) $ error $ "Target is not blank: active = " ++ show active ++ ", rejected = " ++ show (Map.keys rejected)

    forM_ patch $ \(p, pass, fail) ->
        case () of
            _ | pass -> unless (p `elem` unstate (fst active)) $ error "expected pass but not"
              | Just t <- Map.lookup p rejected -> unless (all fail $ Set.toList t) $ error "incorrect test failure"
              | otherwise -> error "missing patch"
    return user


---------------------------------------------------------------------
-- SPECIFIC SIMULATIONS

randomSimple :: IO ()
randomSimple = do
    let info t = mempty{testRequire = [Test "1" | t /= Test "1"]}

    i <- randomRIO (0::Int,10)
    patches <- forM [0..i] $ \i -> do
        j <- randomRIO (0::Int,9)
        return $ Patch $ show i ++ show j
    let failure t p = maybe "0" fromTest t `isSuffixOf` fromPatch p

    let client = Client "c"
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
                let tests = (map Test ["1","2","3"], [])
                return (patches, cont, Reply q good tests)

            _ -> return (patches, cont, Request client)
    putStrLn "Success at randomSimple"


quickPlausible :: IO ()
quickPlausible = do
    let info t = mempty{testPriority = if t == Test "3" then 1 else if t == Test "1" then -1 else 0}
    let client = Client "c"
    let tests = (map Test ["1","2","3","4","5"], [])
    -- start, process 2 tests, add a patch, then process the rest
    -- expect to see 1, X, 1, rest, X

    forM_ [False,True] $ \initialPatch -> do
        done <- simulation info [(client,1)] [] $ \active seen -> return $ case () of
            _ | length seen == 0, initialPatch -> (seen ++ [Test ""], True, Submit (Patch "0") True (const False))
              | length seen == 3 -> (seen ++ [Test ""], True, Submit (Patch "1") True (const False))
              | q:_ <- active -> (maybeToList (qTest q) ++ seen, True, Reply q True tests)
              | otherwise -> (seen ++ [Test "" | null seen], False, Request client)

        case dropWhile null $ map fromTest $ reverse done of
            "3":x:"3":ys
                | [x,"1"] `isSuffixOf` ys
                , sort ys == ["1","2","4","5"]
                -> return ()
            xs -> error $ "quickPlausible wrong test sequence: " ++ show xs
        putStrLn $ "Success at quickPlausible " ++ show initialPatch


bisect :: IO ()
bisect = do
    let info t = mempty
    let tests = (map (Test . show) [1 .. 3 :: Int], [])
    let client = Client "c"
    (done,_) <- simulation info [(client,1)] (0, map (Patch . show) [1 .. 1024 :: Int]) $ \active (done,patches) -> return $ case () of
        _ | p:patches <- patches -> ((done,patches), True, Submit p (p /= Patch "26") (\t -> p == Patch "26" && t == Just (Test "2")))
          | q:_ <- active -> ((done+1,[]), True, Reply q (qTest q /= Just (Test "2") || Patch "26" `notElem` (unstate (fst $ qCandidate q) ++ snd (qCandidate q))) tests)
          | otherwise -> ((done,[]), False, Request client)
    when (done > 50) $ error "Did too many tests to bisect"
    putStrLn "Success at bisect"


performance :: Int -> IO ()
performance nTests = do
    -- TODO: ping the website regularly
    -- 1000 tests, 50 submissions, 7 failing, spawn about every 200 tests
    let nPatches = 50
    let f x = min (nTests-1) $ max 0 $ round $ intToDouble nTests * x 
    let fails = [(3,f 0.2),(4,f 0),(10,f 0.1),(22,f 0.6),(40,f 0.6),(48,f 0.9),(49,f 0.9)]

    let pri = Test $ show $ f 0.1
    let npri = length $ fromTest pri
    let info t = mempty{testPriority = case compare (length $ fromTest t) npri of
                                            LT -> 1; GT -> 0; EQ -> if t < pri then 1 else 0}
    let client = Client "c"
    let tests = (map (Test . show) [0 :: Int .. nTests - 1], [])
    simulation info [(client,3)] (0::Int, 0::Int) $ \active (patch,tick) -> return $ case () of
        _ | tick >= f 0.2, patch < nPatches ->
                let pass = patch `notElem` map fst fails
                    fail t = (patch,maybe (-1) (read . fromTest) t) `elem` fails
                in  ((patch+1, 0), True, Submit (Patch $ show patch) pass fail)
          | q:_ <- active ->
                let pass = and [ (read $ fromPatch p, maybe (-1) (read . fromTest) $ qTest q) `notElem` fails
                               | p <- unstate (fst $ qCandidate q) ++ snd (qCandidate q)]
                in ((patch, tick+1), True, Reply q pass tests)
          | otherwise -> ((patch, tick), patch /= nPatches, Request client)
    putStrLn $ "Success at performance"
    error "stop"
