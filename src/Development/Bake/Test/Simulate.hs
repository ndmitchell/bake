{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Test.Simulate(
    simulate
    ) where

import Development.Bake.Core.Message
import Development.Bake.Core.Type
import Development.Bake.Server.Type
import Development.Bake.Server.Brains
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
    ,server :: Server
    ,wait :: Int
    ,active :: [Question]
    ,asked :: Set.Set Question
    ,patches :: [(Patch, (Bool, Maybe Test -> Bool))]
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
    let ss0 = server0
            {target = (State "", [])
            ,updates=[UpdateInfo t (Answer (strPack "") 0 mempty True) (State "") Nothing]
            ,updatesIdx=Map.singleton (State "") []}
    let s = S u ss0 20 [] Set.empty []

    let count s c = sum [qThreads | Question{..} <- active s, qClient == c]

    let ping s c = brains testInfo (server s) $
            let Just mx = lookup c clients
            in Ping c (fromClient c) mx $ mx - count s c

    let dropPatches ps s = s{patches = filter (flip notElem ps . fst) $ patches s}

    s@S{..} <- flip loopM s $ \s -> do
        putChar '.'

        t <- getCurrentTime
        (u, cont, res) <- step (active s) (user s)
        s <- return s{user = u}
        s <- case res of
            Submit p pass fail -> do
                return s{server = addPatch t "" p $ server s, patches = (p,(pass,fail)) : patches s}

            Reply q good tests -> do
                let ans = Answer (strPack "") 0 (if good && isNothing (qTest q) then tests else mempty) good
                return s{active = delete q $ active s, server = addAnswer q ans $ server s}

            Request c -> case ping s c of
                Sleep -> return $ if cont then s else s{wait = wait s - 1}
                Task q -> do
                    when (q `Set.member` asked s) $ error "asking a duplicate question"
                    return s{active = active s ++ [q]
                            ,asked = Set.insert q $ asked s
                            ,server = addQuestion t q $ server s}
                Update (ss, ps) -> do
                    let nss = restate $ unstate ss ++ ps
                    forM_ ps $ \p -> unless (fst $ fromJust $ lookup p $ patches s) $ error "incorrect test pass"
                    let ans = Answer (strPack "") 0 mempty True
                    return $ dropPatches ps $ s{server = addUpdate t ans (Just nss) (ss,ps) $ server s}

                Reject p t -> do
                    unless (snd (fromJust $ lookup p $ patches s) t) $ error "incorrect test failure"
                    return $ dropPatches [p] $ s{server = rejectPatch p $ server s}

        forM_ clients $ \(c,mx) ->
            when (count s c > mx) $ error "threads exceeded"

        return $ if wait s == 0 then Right s else Left s
    putStrLn ""

    unless (null active) $ error "Active should have been empty"
    unless (null patches) $ error "Patches should have been empty"
    forM_ clients $ \(c,_) ->
        when (ping s c /= Sleep) $ error $ "Brains should have returned sleep,  but returned " ++ show (ping s c)
    when (snd (target server) /= []) $ error $ "Target is not blank: " ++ show (target server)

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
