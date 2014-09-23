{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Development.Bake.Message
import Development.Bake.Server.Type
import Development.Bake.Server.Web
import Control.Concurrent


startServer :: (Show state, Read state, Show patch, Read patch, Show test, Read test)
            => Port -> Author -> String -> Oven state patch test -> IO ()
startServer port author name oven = do
    s <- ovenUpdateState (concrete oven) Nothing
    var <- newMVar $ defaultServer s
    server port $ \p@Payload{..} ->
        if payloadURL == "" then
            web p =<< readMVar var
        else
            fmap Right $ modifyMVar var $ operate (concrete oven) (messageFromPayload p)


operate :: Oven State Patch Test -> Message -> Server -> IO (Server, String)
operate = undefined
