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
import Control.Arrow
import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Data.Time.Clock
import Control.Monad.Extra
import Data.Tuple.Extra
import System.Directory.Extra
import System.IO.Extra
import System.Console.CmdArgs.Verbosity


startServer :: Port -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port author name timeout (concrete -> oven) = do
    s <- withTempDirCurrent $ ovenUpdateState oven Nothing
    putStrLn $ "Initial state of: " ++ show s
    var <- newMVar $ (defaultServer s){authors = [(Nothing,author)]}
    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            res <-
                if null inputURL || ["ui"] `isPrefixOf` inputURL then
                    web oven i{inputURL = drop 1 inputURL} =<< readMVar var
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
    AddPatch author p | Candidate s ps <- active server -> do
        print ("Add patch to",Candidate s $ ps ++ [p])
        now <- getCurrentTime
        dull server{active = Candidate s $ ps ++ [p], authors = (Just p, author) : authors server, submitted = (now,p) : submitted server}
    DelPatch author p | Candidate s ps <- active server -> dull server{active = Candidate s $ delete p ps}
    Pause author -> dull server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author | Candidate s ps <- active server ->
        dull server{paused=Nothing, active = Candidate s $ ps ++ maybe [] (map snd) (paused server)}
    Finished q a -> do
        when (not $ aSuccess a) $ print ("Test failed",qCandidate q == active server,q,a)
        server <- return server{history = [(t,qq,if q == qq then Just a else aa) | (t,qq,aa) <- history server]}
        consistent server
        dull server 
    Pinged ping -> do
        print ping
        now <- getCurrentTime
        server <- return $ prune (addUTCTime (fromRational $ toRational $ negate timeout) now) $ server
            {pings = (now,ping) : filter ((/= pClient ping) . pClient . snd) (pings server)}
        let depends = testRequire . ovenTestInfo oven
        flip loopM server $ \server -> do
            print $ brains depends server ping
            case brains depends server ping of
                Sleep ->
                    return $ Right (server, Nothing)
                Task q -> do
                    when (qClient q /= pClient ping) $ error "client doesn't match the ping"
                    server <- return $ server{history = (now,q,Nothing) : history server}
                    return $ Right (server, Just q)
                Update -> do
                    let Candidate _ ps = active server
                    s <- withTempDirCurrent $ ovenUpdateState oven $ Just $ active server
                    ovenNotify oven [a | (p,a) <- authors server, maybe False (`elem` ps) p] $ unlines
                        ["Your patch just made it in"]
                    return $ Left server{active=Candidate s [], updates=(now,s,active server):updates server}
                Reject p t -> do
                    server <- return $ let Candidate s ps = active server in server{active=Candidate s $ delete p ps}
                    ovenNotify oven [a | (pp,a) <- authors server, Just p == pp] $ unlines
                        ["Your patch " ++ show p ++ " got rejected","Failure in test " ++ show t]
                    return $ Left server
                Broken t -> do
                    let Candidate s ps = active server
                    server <- return $ server{active=Candidate s []}
                    ovenNotify oven [a | (p,a) <- authors server, maybe True (`elem` ps) p] $ unlines
                        ["Eek, it's all gone horribly wrong","Failure with no patches in test " ++ show t]
                    return $ Left server
    where
        dull s = return (s,Nothing)


-- any question that has been asked of a client who hasn't pinged since the time is thrown away
prune :: UTCTime -> Server -> Server
prune cutoff s = s{history = filter (flip elem clients . qClient . snd3) $ history s}
    where clients = [pClient | (t,Ping{..}) <- pings s, t >= cutoff]

consistent :: Server -> IO ()
consistent Server{..} = do
    let xs = groupSort $ map (qCandidate . snd3 &&& id) $ filter (isNothing . qTest . snd3) history
    forM_ xs $ \(c,vs) -> do
        case nub $ map (sort . uncurry (++) . aTests) $ mapMaybe thd3 vs of
            a:b:_ -> error $ "Tests don't match for candidate: " ++ show (c,a,b,vs)
            _ -> return ()


withTempDirCurrent :: IO a -> IO a
withTempDirCurrent act = withTempDir $ \t -> withCurrentDirectory t act
