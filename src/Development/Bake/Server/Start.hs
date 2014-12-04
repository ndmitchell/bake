{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Core.Type
import General.Web
import General.Str
import General.HTML
import Development.Bake.Core.Message
import Development.Bake.Core.Run
import General.Extra
import Development.Bake.Server.Type
import Development.Bake.Server.Web
import Development.Bake.Server.Brains
import General.DelayCache
import Control.DeepSeq
import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Data.Time.Clock
import Control.Monad.Extra
import Data.Tuple.Extra
import System.Console.CmdArgs.Verbosity
import System.FilePath
import qualified Data.Map as Map


startServer :: Port -> FilePath -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port datadir author name timeout (validate . concrete -> oven) = do
    var <- do
        now <- getTimestamp
        putStrLn "Initialising server, computing initial state..."
        (Just state0, answer) <- runInit
        putStrLn $ "Initial state: " ++ fromState state0
        extra <- newDelayCache
        addDelayCache extra (Left state0) $ patchExtra state0 Nothing
        newCVar $ server0
            {target=(state0,[]), authors=Map.fromList [(Nothing,[author])]
            ,logs=[(now,Nothing,answer)]
            ,updates=[((now,answer),state0,Nothing)]
            ,extra=extra
            }

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
                                    AddPatch _ p -> addDelayCache (extra s) (Right p) $ patchExtra (fst $ target s) $ Just p
                                    _ -> return ()
                                operate timeout oven v s
                    )
                else
                    return OutputMissing
            evaluate $ force res


-- | Get information about a patch
patchExtra :: State -> Maybe Patch -> IO (Str, Str)
patchExtra s p = do
    (ex,ans) <- runExtra s p
    let failSummary = renderHTML $ i_ $ str_ "Error when computing patch information"
    let failDetail = renderHTML $ pre_ $ str_ $ strUnpack $ aStdout ans
    return $ fromMaybe (strPack failSummary, strPack failDetail) ex


operate :: Double -> Oven State Patch Test -> Message -> Server -> IO (Server, Maybe Question)
operate timeout oven message server = case message of
    _ | not $ null $ fatal server -> dull server
    AddPatch author p | (s, ps) <- target server -> do
        whenLoud $ print ("Add patch to",s,snoc ps p)
        now <- getTimestamp
        dull server
            {target = (s, filter (/= p) ps `snoc` p)
            ,authors = Map.insertWith (++) (Just p) [author] $ authors server
            ,submitted = (now,p) : submitted server}
    DelPatch author p | (s, ps) <- target server -> dull server{target = (s, delete p ps)}
    Pause author -> dull server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author | (s, ps) <- target server ->
        dull server{paused=Nothing, target = (s, ps ++ maybe [] (map snd) (paused server))}
    Finished q a -> do
        when (not $ aSuccess a) $ do
            putStrLn $ replicate 70 '#'
            print (target server, q, a{aStdout=strPack ""})
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
                Update (s,ps) -> do
                    (Just s2, answer) <- runUpdate s ps
                    ovenNotify oven [a | p <- ps, a <- Map.findWithDefault [] (Just p) $ authors server] $ unlines
                        ["Your patch just made it in"]
                    addDelayCache (extra server) (Left s2) $ patchExtra s2 Nothing
                    return $ Left server{target=(s2, snd (target server) \\ ps), updates=((now,answer),s2,Just (s,ps)):updates server}
                Reject p t -> do
                    ovenNotify oven (Map.findWithDefault [] (Just p) (authors server)) $ unlines
                        ["Your patch " ++ show p ++ " got rejected","Failure in test " ++ show t]
                    return $ Left server{target=second (delete p) $ target server}
                Broken t -> do
                    ovenNotify oven [a | p <- Nothing : map Just (snd $ target server), a <- Map.findWithDefault [] p $ authors server] $ unlines
                        ["Eek, it's all gone horribly wrong","Failure with no patches in test " ++ show t]
                    return $ Right (server{fatal = ("Failure with no patches in test: " ++ maybe "Preparation" fromTest t) : fatal server}, Nothing)
    where
        dull s = return (s,Nothing)
