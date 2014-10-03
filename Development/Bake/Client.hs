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


-- given server, name, threads
startClient :: (Host,Port) -> Author -> String -> Int -> Double -> Oven state patch test -> IO ()
startClient hp author name maxThreads ping (concrete -> oven) = do
    client <- newClient
    queue <- newChan
    nowThreads <- newIORef maxThreads

    forkIO $ do
        readChan queue
        now <- readIORef nowThreads
        q <- sendMessage hp $ Pinged $ Ping client author name maxThreads now
        whenJust q $ \q@Question{..} -> do
            suitable <- maybe (return True) (testSuitable . ovenTestInfo oven) qTest
            if not suitable then do
                sendMessage hp $ Finished q $ Answer "" 0 [] NotApplicable
                writeChan queue ()
             else do
                atomicModifyIORef nowThreads $ \now -> (now - qThreads, ())
                writeChan queue ()
                void $ forkIO $ withTempFile "bake.txt" $ \file -> do
                    (time, (exit, Stdout stdout, Stderr err)) <- timed $
                        cmd "self" (("--output=" ++ file):error "start client")
                    next <- fmap (map undefined . lines) $ readFile file
                    atomicModifyIORef nowThreads $ \now -> (now + qThreads, ())
                    sendMessage hp $ Finished q $
                        Answer stdout time next $ if exit == ExitSuccess then Success else Failure
                    writeChan queue ()

    forever $ writeChan queue () >> sleep ping
