{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Development.Bake.Message
import Development.Bake.Util
import Development.Bake.Server.Type
import Development.Bake.Server.Web
import Development.Bake.Server.Brains
import Development.Shake.Command
import Control.Concurrent
import Control.DeepSeq
import Control.Arrow
import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Data.Time.Clock
import System.Environment.Extra
import Control.Monad.Extra
import Data.Tuple.Extra
import System.Directory.Extra
import System.Console.CmdArgs.Verbosity
import System.FilePath


startServer :: Port -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port author name timeout (concrete -> oven) = do
    exe <- getExecutablePath
    curdirLock <- newMVar ()
    ignore $ removeDirectoryRecursive "bake-server"
    createDirectoryIfMissing True "bake-server"
    s <- withServerDir curdirLock $ ovenUpdateState oven Nothing
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
                        Right v -> do
                            fmap questionToOutput $ modifyMVar var $ \s -> do
                                (s,q) <- operate curdirLock timeout oven v s
                                case v of
                                    AddPatch _ p | p `notElem` map fst (extra s) -> do
                                        forkIO $ do
                                            dir <- createDir "bake-patch" [fromPatch p]
                                            res <- try_ $ do
                                                unit $ cmd (Cwd dir) exe "runextra"
                                                    "--output=extra.txt"
                                                    ["--patch=" ++ fromPatch p]
                                                fmap read $ readFile $ dir </> "extra.txt"
                                            res <- either (fmap dupe . showException) return res
                                            modifyMVar_ var $ \s -> return s{extra = (p,res) : extra s}
                                        return (s{extra=(p,dupe "Calculating..."):extra s}, q)
                                    _ -> return (s,q)
                    )
                else
                    return OutputMissing
            evaluate $ force res


operate :: MVar () -> Double -> Oven State Patch Test -> Message -> Server -> IO (Server, Maybe Question)
operate curdirLock timeout oven message server = case message of
    AddPatch author p | (s, ps) <- active server -> do
        whenLoud $ print ("Add patch to",s,snoc ps p)
        now <- getTimestamp
        dull server{active = (s, snoc ps p), authors = (Just p, author) : authors server, submitted = (now,p) : submitted server}
    DelPatch author p | (s, ps) <- active server -> dull server{active = (s, delete p ps)}
    Pause author -> dull server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author | (s, ps) <- active server ->
        dull server{paused=Nothing, active = (s, ps ++ maybe [] (map snd) (paused server))}
    Finished q a -> do
        when (not $ aSuccess a) $ do
            putStrLn $ replicate 70 '#'
            print (active server, q, a{aStdout=""})
            putStrLn $ aStdout a
            putStrLn $ replicate 70 '#'
        server <- return server{history = [(t,qq,if q == qq then Just a else aa) | (t,qq,aa) <- history server]}
        consistent server
        dull server 
    Pinged ping -> do
        limit <- getCurrentTime
        now <- getTimestamp
        server <- return $ prune (addUTCTime (fromRational $ toRational $ negate timeout) limit) $ server
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
                Update -> do
                    dir <- createDir "bake-test" $ fromState (fst $ active server) : map fromPatch (snd $ active server)
                    s <- withServerDir curdirLock $ withCurrentDirectory (".." </> dir) $
                        ovenUpdateState oven $ Just $ active server
                    ovenNotify oven [a | (p,a) <- authors server, maybe False (`elem` snd (active server)) p] $ unlines
                        ["Your patch just made it in"]
                    return $ Left server{active=(s, []), updates=(now,s,active server):updates server}
                Reject p t -> do
                    ovenNotify oven [a | (pp,a) <- authors server, Just p == pp] $ unlines
                        ["Your patch " ++ show p ++ " got rejected","Failure in test " ++ show t]
                    return $ Left server{active=second (delete p) $ active server}
                Broken t -> do
                    ovenNotify oven [a | (p,a) <- authors server, maybe True (`elem` snd (active server)) p] $ unlines
                        ["Eek, it's all gone horribly wrong","Failure with no patches in test " ++ show t]
                    return $ Left server{active=(fst $ active server, [])}
    where
        dull s = return (s,Nothing)


-- any question that has been asked of a client who hasn't pinged since the time is thrown away
prune :: UTCTime -> Server -> Server
prune cutoff s = s{history = filter (flip elem clients . qClient . snd3) $ history s}
    where clients = [pClient | (Timestamp t _,Ping{..}) <- pings s, t >= cutoff]

consistent :: Server -> IO ()
consistent Server{..} = do
    let xs = groupSort $ map (qCandidate . snd3 &&& id) $ filter (isNothing . qTest . snd3) history
    forM_ xs $ \(c,vs) -> do
        case nub $ map (sort . uncurry (++) . aTests) $ filter aSuccess $ mapMaybe thd3 vs of
            a:b:_ -> error $ "Tests don't match for candidate: " ++ show (c,a,b,vs)
            _ -> return ()


withServerDir :: MVar () -> IO a -> IO a
withServerDir curdirLock act = withMVar curdirLock $ const $ withCurrentDirectory "bake-server" act
