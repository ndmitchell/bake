{-# LANGUAGE RecordWildCards, ViewPatterns, ScopedTypeVariables #-}

module Development.Bake.Client(
    startClient
    ) where

import Development.Bake.Type
import Development.Bake.Message
import System.Exit
import Control.Exception.Extra
import Development.Shake.Command
import Control.Concurrent
import Control.Monad.Extra
import System.Time.Extra
import Data.IORef
import Data.Maybe
import System.Environment
import System.Directory


-- given server, name, threads
startClient :: (Host,Port) -> Author -> String -> Int -> Double -> Oven state patch test -> IO ()
startClient hp author (Client -> client) maxThreads ping (concrete -> oven) = do
    queue <- newChan
    nowThreads <- newIORef maxThreads

    root <- myThreadId
    exe <- getExecutablePath
    let safeguard = handle_ (throwTo root)
    forkIO $ safeguard $ forever $ do
        readChan queue
        now <- readIORef nowThreads
        q <- sendMessage hp $ Pinged $ Ping client author maxThreads now
        whenJust q $ \q@Question{..} -> do
            atomicModifyIORef nowThreads $ \now -> (now - qThreads, ())
            writeChan queue ()
            void $ forkIO $ safeguard $ do
                dir <- candidateDir qCandidate
                (time, (exit, Stdout sout, Stderr serr)) <- duration $
                    cmd (Cwd dir) exe "runtest"
                        "--output=../tests.txt"
                        ["--test=" ++ fromTest t | Just t <- [qTest]]
                        ("--state=" ++ fromState (fst qCandidate))
                        ["--patch=" ++ fromPatch p | p <- snd qCandidate]
                tests <- if isJust qTest || exit /= ExitSuccess then return ([],[]) else do
                    src ::  ([String],[String]) <- fmap read $ readFile "tests.txt"
                    let op = map (stringyFrom (ovenStringyTest oven))
                    putStrLn "FIXME: Should validate the next set forms a DAG"
                    return (op (fst src), op (snd src))
                atomicModifyIORef nowThreads $ \now -> (now + qThreads, ())
                sendMessage hp $ Finished q $
                    Answer (sout++serr) time tests $ exit == ExitSuccess
                writeChan queue ()

    forever $ writeChan queue () >> sleep ping


-- | Find a directory for this patch
candidateDir :: (State, [Patch]) -> IO FilePath
candidateDir (s, ps) = do
    let file = "candidates.txt"
    let c_ = (fromState s, map fromPatch ps)
    b <- doesFileExist file
    src :: [((String, [String]), FilePath)] <- if b then fmap read $ readFile file else return []
    case lookup c_ src of
        Just p -> return p
        Nothing -> do
            let res = show $ length src
            createDirectoryIfMissing True res
            writeFile "candidates.txt" $ show $ (c_,res):src
            return res
