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
import Development.Bake.Server.Stats
import Control.Applicative
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
        extra <- newDelayCache
        putStrLn "Initialising server, computing initial state..."
        (res, answer) <- runInit
        when (isNothing res) $
            ovenNotify oven [author] "Failed to initialise, pretty serious"
        let state0 = fromMaybe sFailure res
        putStrLn $ "Initial state: " ++ maybe "!FAILURE!" fromState res
        when (isJust res) $ addDelayCache extra (Left state0) $ patchExtra state0 Nothing
        newCVar $ server0
            {target=(state0,[]), authors=Map.fromList [(Nothing,[author])]
            ,updates=[((now,answer),state0,Nothing)]
            ,extra=extra
            ,fatal=["Failed to initialise" | isNothing res]
            }

    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            res <-
                if null inputURL then
                    fmap OutputHTML $ recordIO "web" $ web oven inputArgs =<< readCVar var
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
    AddPatch author p -> do
        let add ps = filter (/= p) ps `snoc` p
        now <- getTimestamp
        if p `elem` concatMap (maybe [] snd . thd3) (updates server) then
            -- gets confusing if a patch is both included AND active
            dull server
         else
            dull server
                {target = second (if isJust (paused server) then id else add) $ target server
                ,paused = (if p `elem` snd (target server) then id else add) <$> paused server
                ,authors = Map.insertWith (++) (Just p) [author] $ authors server
                ,submitted = (now,p) : submitted server}
    DelPatch author p -> do
        dull $ unpause server
            {target = second (delete p) $ target server
            ,paused = delete p <$> paused server
            }
    DelAllPatches author ->
        dull $ server{paused = Nothing, target = (fst $ target server, [])}
    Pause author ->
        -- cannot pause if there is no work outstanding, unpause may immediately undo
        dull $ unpause $ server{paused = Just $ fromMaybe [] $ paused server}
    Unpause author ->
        dull server{paused=Nothing, target = second (++ fromMaybe [] (paused server)) $ target server}
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
        flip loopM server $ \(unpause -> server) ->
            case record "brains" (brains (ovenTestInfo oven) server) ping of
                Sleep ->
                    return $ Right (server, Nothing)
                Task q -> do
                    when (qClient q /= pClient ping) $ error "client doesn't match the ping"
                    server <- return $ server{history = (now,q,Nothing) : history server}
                    return $ Right (server, Just q)
                Update (s,ps) -> do
                    (s2, answer) <- runUpdate s ps
                    case s2 of
                        Nothing -> do
                            ovenNotify oven [a | p <- Nothing : map Just (snd $ target server), a <- Map.findWithDefault [] p $ authors server]
                                "Failed to update, pretty serious"
                            return $ Right (server{fatal="Failed to update":fatal server, updates=((now,answer),sFailure,Just (s,ps)):updates server}, Nothing)
                        Just s2 -> do
                            ovenNotify oven [a | p <- ps, a <- Map.findWithDefault [] (Just p) $ authors server]
                                "Your patch just made it in"
                            addDelayCache (extra server) (Left s2) $ patchExtra s2 Nothing
                            return $ Left server{target=(s2, snd (target server) \\ ps), updates=((now,answer),s2,Just (s,ps)):updates server}
                Reject p t -> do
                    ovenNotify oven (Map.findWithDefault [] (Just p) (authors server)) $ unlines
                        ["Your patch " ++ show p ++ " got rejected","Failure in test " ++ show t]
                    return $ Left server{target=second (delete p) $ target server}
    where
        dull s = return (s,Nothing)
        unpause server
            | null $ snd $ target server = server
                {paused = Nothing
                ,target = second (++ fromMaybe [] (paused server)) $ target server}
            | otherwise = server


sFailure = State ""
