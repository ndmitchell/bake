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
import System.IO


-- Properties!
-- * By the time you finish, every patch must be propertly rejected or accepted
-- * Clients must not be executing too much at one time

data S = S
    {server :: Server
    ,wait :: Int
    ,active :: [Question]
    ,answered :: [Question]
    ,patches :: [Patch]
    ,rejected :: [Patch]
    ,accepted :: [Patch]
    }

simulate :: IO ()
simulate = do
    let info t = mempty{testRequire = [Test "1" | t /= Test "1"]}
    let maxThreads = 2
    i <- randomRIO (0::Int,10)
    ps <- forM [0..i] $ \i -> do
        j <- randomRIO (0::Int,9)
        return $ Patch $ show i ++ show j

    let s = S (Server mempty mempty mempty (State "", []) mempty mempty mempty (error "no extra")) 200 [] [] ps [] []
    let ping s = brains info (server s) $ Ping (Client "x") "x" maxThreads (maxThreads - sum (map qThreads $ active s))
    let failure t p = maybe "0" fromTest t `isSuffixOf` fromPatch p
    let unstate = map Patch . words . fromState
    let restate = State . unwords . map fromPatch
    hSetBuffering stdout NoBuffering
    S{..} <- flip loopM s $ \s -> do
        putChar '.'
        -- enqueue a patch
        i <- randomRIO (0::Int,if wait s > 100 then 20 else 0)
        s <- if null (patches s) || i /= 0 then return s else do
            let p:ps = patches s
            let f x = x{target = second (`snoc` p) $ target x}
            return s{server = f $ server s, patches = ps}

        -- answer a question
        i <- randomRIO (0::Int,10)
        s <- if null (active s) || i /= 0 then return s else do
            i <- randomRIO (0, length (active s) - 1)
            let (pre, q:post) = splitAt i $ active s
            let good = not $ any (failure $ qTest q) $ unstate (fst $ qCandidate q) ++ snd (qCandidate q)
            let ans = Answer (strPack "") 0 (if good && isNothing (qTest q) then (map Test ["1","2","3"], []) else mempty) good
            let f xs = [(t,qq,if q == qq then Just ans else aa) | (t,qq,aa) <- xs]
            return s{active = pre++post, answered = q : answered s, server = (server s){history = f $ history $ server s}}
        t <- getTimestamp
        s <- case ping s of
            Sleep -> return s
            Task q -> do
                when (q `elem` answered s ++ active s) $ error "asking a duplicate question"
                when (qThreads q + sum (map qThreads $ active s) > maxThreads) $ error "threads exceeded"
                return s{active = q : active s, server = (server s){history = (t,q,Nothing) : history (server s)} }
            Update (ss, ps) -> do
                let (oss, ops) = target $ server s
                let (nss, nps) = (restate $ unstate ss ++ ps, ops \\ ps)
                return s{accepted = ps ++ accepted s
                        ,server = (server s){target = (nss,nps), updates = (t, nss, (oss, ops)) : updates (server s)}}
            Reject p t -> do
                unless (failure t p) $ error "incorrect test failure"
                return s{rejected = p : rejected s, server = (server s){target = second (delete p) $ target $ server s}}
            Broken _ -> error "ended up at broken"

        s <- return $ if null (active s) && null (patches s) then s{wait = wait s - 1} else s
        return $ if wait s == 0 then Right s else Left s

    when (active /= []) $ error "Active should have been empty"
    when (sort ps /= sort (rejected ++ accepted)) $ error "Some patches are missing"
    res <- return $ brains info server $ Ping (Client "x") "x" maxThreads maxThreads
    when (res /= Sleep) $ error $ "Brains should have returned sleep,  but returned " ++ show res

    putStrLn "\nSuccess"
