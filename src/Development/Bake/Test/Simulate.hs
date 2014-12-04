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
import General.Str
import General.Extra
import System.Random
import System.IO.Extra


simulate :: IO ()
simulate = withBuffering stdout NoBuffering $ do
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
    ,asked :: [Question]
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
    let s = S u server0{target = (State "", [])} 200 [] [] []

    let count s c = sum [qThreads | Question{..} <- active s, qClient == c]

    let ping s c = brains testInfo (server s) $
            let Just mx = lookup c clients
            in Ping c (fromClient c) mx $ mx - count s c

    let dropPatches ps s = s{patches = filter (flip notElem ps . fst) $ patches s}

    s@S{..} <- flip loopM s $ \s -> do
        putChar '.'

        t <- getTimestamp
        (u, cont, res) <- step (active s) (user s)
        s <- return s{user = u}
        s <- case res of
            Submit p pass fail -> do
                let f x = x{target = second (`snoc` p) $ target x}
                return s{server = f $ server s, patches = (p,(pass,fail)) : patches s}

            Reply q good tests -> do
                let ans = Answer (strPack "") 0 (if good && isNothing (qTest q) then tests else mempty) good
                let f xs = [(t,qq,if q == qq then Just ans else aa) | (t,qq,aa) <- xs]
                return s{active = delete q $ active s, server = (server s){history = f $ history $ server s}}

            Request c -> case ping s c of
                Sleep -> return s
                Task q -> do
                    when (q `elem` asked s) $ error "asking a duplicate question"
                    return s{active = q : active s, server = (server s){history = (t,q,Nothing) : history (server s)} }
                Update (ss, ps) -> do
                    let (nss, nps) = (restate $ unstate ss ++ ps, snd (target $ server s) \\ ps)
                    forM_ ps $ \p -> unless (fst $ fromJust $ lookup p $ patches s) $ error "incorrect test pass"
                    return $ dropPatches ps $ s{server = (server s){target = (nss,nps), updates = (t, nss, (ss, ps)) : updates (server s)}}
                Reject p t -> do
                    unless (snd (fromJust $ lookup p $ patches s) t) $ error "incorrect test failure"
                    return $ dropPatches [p] $ s{server = (server s){target = second (delete p) $ target $ server s}}
                Broken _ -> error "ended up at broken"

        forM_ clients $ \(c,mx) ->
            when (count s c > mx) $ error "threads exceeded"

        s <- return $ if cont then s else s{wait = wait s - 1}
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
