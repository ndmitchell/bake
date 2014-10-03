{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.Client(
    startClient
    ) where

import Development.Bake.Type
import Development.Bake.Util
import Development.Bake.Message
import System.Exit
import Development.Shake.Command
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Environment


-- given server, name, threads
startClient :: (Host,Port) -> Author -> String -> Int -> Double -> Oven state patch test -> IO ()
startClient hp author name maxThreads ping (concrete -> oven) = do
    client <- newClient
    queue <- newChan
    nowThreads <- newIORef maxThreads

    root <- myThreadId
    exe <- getExecutablePath
    let safeguard = handle_ (throwTo root)
    forkIO $ safeguard $ forever $ do
        readChan queue
        now <- readIORef nowThreads
        q <- sendMessage hp $ Pinged $ Ping client author name maxThreads now
        whenJust q $ \q@Question{qCandidate=Candidate qState qPatches,..} -> do
            suitable <- maybe (return True) (testSuitable . ovenTestInfo oven) qTest
            if not suitable then do
                sendMessage hp $ Finished q $ Answer "" 0 [] NotApplicable
                writeChan queue ()
             else do
                atomicModifyIORef nowThreads $ \now -> (now - qThreads, ())
                writeChan queue ()
                void $ forkIO $ safeguard $ withTempFile "bake.txt" $ \file -> do
                    (time, (exit, Stdout sout, Stderr serr)) <- duration $
                        cmd exe "run"
                            ("--output=" ++ file)
                            ["--test=" ++ fromTest t | Just t <- [qTest]]
                            ("--state=" ++ fromState qState)
                            ["--patch=" ++ fromPatch p | p <- qPatches]
                    next <- fmap (map (error "client read") . lines) $ readFile file
                    putStrLn "FIXME: Should validate the next set forms a DAG"
                    atomicModifyIORef nowThreads $ \now -> (now + qThreads, ())
                    sendMessage hp $ Finished q $
                        Answer (sout++serr) time next $ if exit == ExitSuccess then Success else Failure
                    writeChan queue ()

    forever $ writeChan queue () >> sleep ping
