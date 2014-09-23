{-# LANGUAGE RecordWildCards, TupleSections #-}

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
import Control.Monad
import Data.List
import Data.Maybe


startServer :: (Show state, Read state, Show patch, Read patch, Show test, Read test)
            => Port -> Author -> String -> Oven state patch test -> IO ()
startServer port author name oven = do
    s <- ovenUpdateState (concrete oven) Nothing
    var <- newMVar $ defaultServer s
    forkIO $ forever $ do
        sleep 60
        modifyMVar_ var heartbeat
    server port $ \p@Payload{..} ->
        if payloadURL == "" then
            web p =<< readMVar var
        else do
            modifyMVar var $ operate (concrete oven) (messageFromPayload p)
            return $ Right ""


operate :: Oven State Patch Test -> Message -> Server -> IO (Server, [Reply])
operate oven message server = return $ (,[]) $ case message of
    AddPatch author p | Candidate s ps <- active server -> server{active = Candidate s (ps ++ [p])}
    DelPatch author p | Candidate s ps <- active server -> server{active = Candidate s $ delete p ps}
    Pause author -> server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author | Candidate s ps <- active server -> server{paused=Nothing, active = Candidate s $ ps ++ fromMaybe [] (paused server)}
    Finished a b c d e f -> server{history = History a c d e f : history server}
    Ping author name cookie provides threads -> error "todo: assign a task"


heartbeat :: Server -> IO Server
heartbeat = undefined
