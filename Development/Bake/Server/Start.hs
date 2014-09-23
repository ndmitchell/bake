{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Data.IORef
import Development.Bake.Server.Type


startServer :: (Show state, Read state, Show patch, Read patch, Show test, Read test)
            => Port -> Author -> String -> Oven state patch test -> IO ()
startServer port author name oven = do
    s <- ovenUpdateState (concrete oven) Nothing
    ref <- newIORef $ defaultServer s
    server port $ \Payload{..} -> undefined
