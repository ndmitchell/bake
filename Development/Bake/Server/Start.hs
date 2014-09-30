{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Development.Bake.Util
import Development.Bake.Message
import Development.Bake.Server.Type
import Development.Bake.Server.Web
import Control.Concurrent
import Data.List
import Data.Maybe


startServer :: Port -> Author -> String -> Oven state patch test -> IO ()
startServer port author name (concrete -> oven) = do
    s <- withTempDirCurrent $ ovenUpdateState oven Nothing
    putStrLn $ "Initial state of: " ++ show s
    var <- newMVar $ defaultServer s
    server port $ \i@Input{..} ->
        if null inputURL || ["ui"] `isPrefixOf` inputURL then
            web i{inputURL = drop 1 inputURL} =<< readMVar var
        else if ["api"] `isPrefixOf` inputURL then
            (case messageFromInput i{inputURL = drop 1 inputURL} of
                Left e -> return $ OutputError e
                Right v -> fmap questionsToOutput $ modifyMVar var $ operate oven v
            )
        else
            return OutputMissing


operate :: Oven State Patch Test -> Message -> Server -> IO (Server, [Question])
operate oven message server = return $ (,[]) $ case message of
    AddPatch author p | Candidate s ps <- active server -> server{active = Candidate s (ps ++ [p])}
    DelPatch author p | Candidate s ps <- active server -> server{active = Candidate s $ delete p ps}
    Pause author -> server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author | Candidate s ps <- active server -> server{paused=Nothing, active = Candidate s $ ps ++ fromMaybe [] (paused server)}
    Finished q a -> server{history = (q, Just a) : delete (q, Nothing) (history server)}
    Pinged Ping{..} -> error "todo: assign a task"

