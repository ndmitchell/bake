{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Core.Type
import General.Web
import General.Str
import Development.Bake.Core.Message
import General.Extra
import Development.Bake.Server.Type
import Development.Bake.Server.Web
import Development.Bake.Server.Brains
import General.DelayCache
import Development.Shake.Command
import Control.DeepSeq
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
import qualified Data.Map as Map


startServer :: Port -> FilePath -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port datadir author name timeout (validate . concrete -> oven) = do
    state0 <- initialState oven
    var <- do
        extra <- newDelayCache
        newCVar $ Server [] [] Map.empty (state0,[]) [] Nothing [] (Map.fromList [(Nothing,[author])]) extra
    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            res <-
                if null inputURL then
                    web oven inputArgs =<< readCVar var
                else if ["html"] `isPrefixOf` inputURL then
                    return $ OutputFile $ datadir </> "html" </> last inputURL
                else if ["api"] `isPrefixOf` inputURL then
                    (case messageFromInput i{inputURL = drop 1 inputURL} of
                        Left e -> return $ OutputError e
                        Right v -> do
                            fmap questionToOutput $ modifyCVar var $ \s -> do
                                case v of
                                    AddPatch _ p -> addDelayCache (extra s) (Right p) $ patchExtra (fst $ active s) $ Just p
                                    _ -> return ()
                                operate timeout oven v s
                    )
                else
                    return OutputMissing
            evaluate $ force res


-- | Get information about a patch
patchExtra :: State -> Maybe Patch -> IO (Str, Str)
patchExtra s p = do
    exe <- getExecutablePath
    dir <- createDir "bake-extra" $ fromState s : maybeToList (fmap fromPatch p)
    res <- try_ $ do
        unit $ cmd (Cwd dir) exe "runextra"
            "--output=extra.txt"
            ["--state=" ++ fromState s]
            ["--patch=" ++ fromPatch p | Just p <- [p]]
        fmap read $ readFile $ dir </> "extra.txt"
    fmap (both strPack) $ either (fmap dupe . showException) return res


operate :: Double -> Oven State Patch Test -> Message -> Server -> IO (Server, Maybe Question)
operate timeout oven message server = case message of
    AddPatch author p | (s, ps) <- active server -> do
        whenLoud $ print ("Add patch to",s,snoc ps p)
        now <- getTimestamp
        dull server
            {active = (s, snoc ps p)
            ,authors = Map.insertWith (++) (Just p) [author] $ authors server
            ,submitted = (now,p) : submitted server}
    DelPatch author p | (s, ps) <- active server -> dull server{active = (s, delete p ps)}
    Pause author -> dull server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author | (s, ps) <- active server ->
        dull server{paused=Nothing, active = (s, ps ++ maybe [] (map snd) (paused server))}
    Finished q a -> do
        when (not $ aSuccess a) $ do
            putStrLn $ replicate 70 '#'
            print (active server, q, a{aStdout=strPack ""})
            putStrLn $ strUnpack $ aStdout a
            putStrLn $ replicate 70 '#'
        server <- return server{history = [(t,qq,if q == qq then Just a else aa) | (t,qq,aa) <- history server]}
        serverConsistent server
        dull server 
    Pinged ping -> do
        limit <- getCurrentTime
        now <- getTimestamp
        server <- return $ serverPrune (addUTCTime (fromRational $ toRational $ negate timeout) limit) $ server
            {pings = Map.insert (pClient ping) (now,ping) $ pings server}
        flip loopM server $ \server ->
            case brains (ovenTestInfo oven) server ping of
                Sleep ->
                    return $ Right (server, Nothing)
                Task q -> do
                    when (qClient q /= pClient ping) $ error "client doesn't match the ping"
                    server <- return $ server{history = (now,q,Nothing) : history server}
                    return $ Right (server, Just q)
                Update -> do
                    dir <- createDir "bake-test" $ fromState (fst $ active server) : map fromPatch (snd $ active server)
                    s <- withCurrentDirectory dir $
                        ovenUpdateState oven $ Just $ active server
                    ovenNotify oven [a | p <- snd $ active server, a <- Map.findWithDefault [] (Just p) $ authors server] $ unlines
                        ["Your patch just made it in"]
                    return $ Left server{active=(s, []), updates=(now,s,active server):updates server}
                Reject p t -> do
                    ovenNotify oven (Map.findWithDefault [] (Just p) (authors server)) $ unlines
                        ["Your patch " ++ show p ++ " got rejected","Failure in test " ++ show t]
                    return $ Left server{active=second (delete p) $ active server}
                Broken t -> do
                    ovenNotify oven [a | p <- Nothing : map Just (snd $ active server), a <- Map.findWithDefault [] p $ authors server] $ unlines
                        ["Eek, it's all gone horribly wrong","Failure with no patches in test " ++ show t]
                    return $ Left server{active=(fst $ active server, [])}
    where
        dull s = return (s,Nothing)


initialState :: Oven State Patch Test -> IO State
initialState oven = do
    putStrLn "Initialising server, computing initial state..."
    ignore $ removeDirectoryRecursive "bake-server"
    createDirectoryIfMissing True "bake-server"
    s <- withCurrentDirectory "bake-server" $ ovenUpdateState oven Nothing
    putStrLn $ "Initial state: " ++ fromState s
    return s
