
-- | Define a continuous integration system.
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


-- given server, name, threads
startClient :: (Host,Port) -> Author -> String -> [String] -> Int -> IO ()
startClient hp author name provides threads = do
    cookie <- newCookie

    let process xs = do
            forM_ xs $ \(Reply can test) -> forkIO $ withTempFile "bake.txt" $ \file -> do
                (time, (exit, Stdout stdout)) <- timed $ cmd "self" (("--output=" ++ file):error "start client")
                info <- case exit of
                    ExitFailure i -> return $ Left i
                    ExitSuccess -> fmap (Right . map Test . lines) $ readFile file
                sendMessage hp $ Finished can cookie test stdout time info
                ping

        ping = process =<< sendMessage hp (Ping author name cookie provides threads)

    forever $ ping >> sleep 60
