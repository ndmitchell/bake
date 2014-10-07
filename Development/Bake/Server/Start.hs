{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Development.Bake.Message
import Development.Bake.Server.Type
import Development.Bake.Server.Web
import Development.Bake.Server.Brains
import Control.Concurrent
import Control.DeepSeq
import Control.Exception.Extra
import Data.List
import Data.Maybe
import Data.Time.Clock
import Control.Monad.Extra
import Data.Tuple.Extra
import System.Directory.Extra
import System.IO.Extra


startServer :: Port -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port author name timeout (concrete -> oven) = do
    s <- withTempDirCurrent $ ovenUpdateState oven Nothing
    putStrLn $ "Initial state of: " ++ show s
    var <- newMVar $ defaultServer s
    server port $ \i@Input{..} -> do
        print i
        handle_ (fmap OutputError . showException) $ do
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
        server <- return server{history = [(t,qq,if q == qq then Just a else aa) | (t,qq,aa) <- history server]}
        consistent server
        mapM_ (uncurry $ ovenNotify oven) $ notify q a server
        dull server 
    Pinged ping -> do
        now <- getCurrentTime
        server <- return $ prune (addUTCTime (fromRational $ toRational $ negate timeout) now) $ server
            {pings = (now,ping) : filter ((/= pClient ping) . pClient . snd) (pings server)}
        let depends = testRequire . ovenTestInfo oven
        flip loopM server $ \server ->
            case brains depends server ping of
                Sleep ->
                    return $ Right (server, Nothing)
                Task q -> do
                    when (qClient q /= pClient ping) $ error "client doesn't match the ping"
                    server <- return $ server{history = (now,q,Nothing) : history server}
                    return $ Right (server, Just q)
                Update c -> do
                    s <- withTempDirCurrent $ ovenUpdateState oven $ Just $ active server
                    return $ Left server{active=Candidate s [], updates=(s,active server):updates server}
                Reject p t -> do
                    server <- return $ let Candidate s ps = active server in server{active=Candidate s $ delete p ps}
                    return $ Left server
                Broken s ps -> do
                    error "failure in zero mode, eek, it's all gone wrong"
    where
        dull s = return (s,Nothing)


notify :: Question -> Answer -> Server -> [(Author, String)]
notify _ _ _ = [] -- FIXME: sometimes tell someone something


-- any question that has been asked of a client who hasn't pinged since the time is thrown away
prune :: UTCTime -> Server -> Server
prune cutoff s = s{history = filter (flip elem clients . qClient . snd3) $ history s}
    where clients = [pClient | (t,Ping{..}) <- pings s, t >= cutoff]

consistent :: Server -> IO ()
consistent Server{..} = do
    putStrLn "FIXME: Check all for a given Candidate, all aTest answers give the same set of results"

withTempDirCurrent :: IO a -> IO a
withTempDirCurrent act = withTempDir $ \t -> withCurrentDirectory t act
