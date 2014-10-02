{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns, ScopedTypeVariables #-}

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
import Development.Bake.Server.Brains
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Time.Clock


startServer :: Port -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port author name timeout (concrete -> oven) = do
    s <- withTempDirCurrent $ ovenUpdateState oven Nothing
    putStrLn $ "Initial state of: " ++ show s
    var <- newMVar $ defaultServer s
    server port $ \i@Input{..} ->
        handle (\(e :: SomeException) -> fmap OutputError $ showException e) $ do
            res <-
                if null inputURL || ["ui"] `isPrefixOf` inputURL then
                    web i{inputURL = drop 1 inputURL} =<< readMVar var
                else if ["api"] `isPrefixOf` inputURL then
                    (case messageFromInput i{inputURL = drop 1 inputURL} of
                        Left e -> return $ OutputError e
                        Right v -> fmap questionToOutput $ modifyMVar var $ operate timeout oven v
                    )
                else
                    return OutputMissing
            evaluate $ force res


operate :: Double -> Oven State Patch Test -> Message -> Server -> IO (Server, Maybe Question)
operate timeout oven message server = case message of
    AddPatch author p | Candidate s ps <- active server -> dull server{active = Candidate s $ ps ++ [p]}
    DelPatch author p | Candidate s ps <- active server -> dull server{active = Candidate s $ delete p ps}
    Pause author -> dull server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author | Candidate s ps <- active server ->
        dull server{paused=Nothing, active = Candidate s $ ps ++ fromMaybe [] (paused server)}
    Finished q a -> do
        server <- return server{history = (q, Just a) : delete (q, Nothing) (history server)}
        mapM_ (uncurry $ ovenNotify oven) $ notify q a server
        dull server 
    Pinged ping -> do
        now <- getCurrentTime
        let (q,upd) = brains (addUTCTime (fromRational $ toRational $ negate timeout) now) server ping
        server <- return $ server
            {history = map (,Nothing) (maybeToList q) ++ history server
            ,pings = ping : deleteBy ((==) `on` pClient) ping (pings server)}
        when upd $ error $ "operate, update"
        return (server, fmap (\q -> q{qStarted = now, qClient = pClient ping}) q)
    where
        dull s = return (s,Nothing)


notify :: Question -> Answer -> Server -> [(Author, String)]
notify _ _ _ = [] -- FIXME: sometimes tell someone something
