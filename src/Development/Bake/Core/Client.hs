{-# LANGUAGE RecordWildCards, ViewPatterns, ScopedTypeVariables #-}

module Development.Bake.Core.Client(
    startClient
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Run
import General.Extra
import General.Str
import Development.Bake.Core.Message
import Control.Concurrent
import Control.Monad.Extra
import System.Time.Extra
import Data.IORef
import Data.Tuple.Extra
import System.Environment.Extra


-- given server, name, threads
startClient :: (Host,Port) -> Author -> String -> Int -> Double -> Oven state patch test -> IO ()
startClient hp author (Client -> client) maxThreads ping (validate . concrete -> oven) = do
    when (client == Client "") $ error "You must give a name to the client, typically with --name"
    queue <- newChan
    nowThreads <- newIORef maxThreads

    unique <- newIORef 0
    root <- myThreadId
    exe <- getExecutablePath
    forkSlave $ forever $ do
        readChan queue
        now <- readIORef nowThreads
        q <- sendMessage hp $ Pinged $ Ping client author maxThreads now
        whenJust q $ \q@Question{..} -> do
            atomicModifyIORef nowThreads $ \now -> (now - qThreads, ())
            writeChan queue ()
            void $ forkSlave $ do
                i <- atomicModifyIORef unique $ dupe . succ
                putBlock "Client start" $
                    ["Client: " ++ fromClient client
                    ,"Id: " ++ show i
                    ,"Test: " ++ maybe "Prepare" fromTest qTest
                    ,"State: " ++ fromState (fst qCandidate)
                    ,"Patches:"] ++
                    map ((++) "    " . fromPatch) (snd qCandidate)
                a@Answer{..} <- runTest (fst qCandidate) (snd qCandidate) qTest
                putBlock "Client stop" $
                    ["Client: " ++ fromClient client
                    ,"Id: " ++ show i
                    ,"Result: " ++ (if aSuccess then "Success" else "Failure")
                    ,"Duration: " ++ showDuration aDuration
                    ,"Output: " ++ strUnpack aStdout
                    ]
                atomicModifyIORef nowThreads $ \now -> (now + qThreads, ())
                sendMessage hp $ Finished q a
                writeChan queue ()

    forever $ writeChan queue () >> sleep ping
