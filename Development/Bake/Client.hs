{-# LANGUAGE RecordWildCards, ViewPatterns, ScopedTypeVariables #-}

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
        whenJust q $ \q@Question{qCandidate=qCandidate@(Candidate qState qPatches),..} -> do
            suitable <- maybe (return True) (testSuitable . ovenTestInfo oven) qTest
            if not suitable then do
                sendMessage hp $ Finished q $ Answer "" 0 [] NotApplicable
                writeChan queue ()
             else do
                atomicModifyIORef nowThreads $ \now -> (now - qThreads, ())
                writeChan queue ()
                void $ forkIO $ safeguard $ do
                    dir <- candidateDir qCandidate
                    (time, (exit, Stdout sout, Stderr serr)) <- duration $
                        cmd (Cwd dir) exe "run"
                            "--output=../tests.txt"
                            ["--test=" ++ fromTest t | Just t <- [qTest]]
                            ("--state=" ++ fromState qState)
                            ["--patch=" ++ fromPatch p | p <- qPatches]
                    next <- if isJust qTest || exit /= ExitSuccess then return [] else
                        fmap (map (stringyFrom (ovenStringyTest oven)) . lines) $ readFile "tests.txt"
                    putStrLn "FIXME: Should validate the next set forms a DAG"
                    atomicModifyIORef nowThreads $ \now -> (now + qThreads, ())
                    sendMessage hp $ Finished q $
                        Answer (sout++serr) time next $ if exit == ExitSuccess then Success else Failure
                    writeChan queue ()

    forever $ writeChan queue () >> sleep ping


-- | Find a directory for this patch
candidateDir :: Candidate State Patch -> IO FilePath
candidateDir (Candidate s ps) = do
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
