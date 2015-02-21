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
import General.DelayCache
import Control.DeepSeq
import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Data.Char
import Control.Monad.Extra
import Data.Tuple.Extra
import System.Directory
import System.Console.CmdArgs.Verbosity
import System.FilePath
import qualified Data.Map as Map


startServer :: Port -> FilePath -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port datadir author name timeout (validate . concrete -> oven) = do
    do
        dir <- getCurrentDirectory
        strInit (dir </> "bake-string") (25 * 1024 * 1024) -- use at most 25Mb for strings
    var <- do
        now <- getCurrentTime
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
            ,updates=[UpdateInfo now answer state0 Nothing]
            ,extra=extra
            ,fatal=["Failed to initialise" | isNothing res]
            }

    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            res <-
                if null inputURL then
                    fmap OutputHTML $ web oven inputArgs =<< readCVar var
                else if ["html"] `isPrefixOf` inputURL then
                    return $ OutputFile $ datadir </> "html" </> last inputURL
                else if ["api"] `isPrefixOf` inputURL then
                    (case messageFromInput i{inputURL = drop 1 inputURL} of
                        Left e -> return $ OutputError e
                        Right v -> do
                            evaluate $ rnf v
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
        now <- getCurrentTime
        dull $ addPatch now author p server
    DelPatch author p -> do
        dull $ deletePatch p server
    DelAllPatches author ->
        dull $ clearPatches server
    Pause author ->
        -- cannot pause if there is no work outstanding, unpause may immediately undo
        dull $ startPause server
    Unpause author ->
        dull $ stopPause server
    Finished q a -> do
        when (not $ aSuccess a) $ do
            putStrLn $ replicate 70 '#'
            print (target server, q, a{aStdout=strPack ""})
            putStrLn $ strUnpack $ aStdout a
            putStrLn $ replicate 70 '#'
        server <- return $ addAnswer q a server
        serverConsistent server
        dull server 
    Pinged ping -> do
        now <- getCurrentTime
        server <- return $ serverPrune (addSeconds (negate timeout) now) $
            addPing now ping server
        flip loopM server $ \(ensurePauseInvariants -> server) -> do
            let neuronName x = ["brains", lower $ takeWhile (not . isSpace) $ show x]
            case record ((neuronName &&& id) . brains (ovenTestInfo oven) server) ping of
                Sleep ->
                    return $ Right (server, Nothing)
                Task q -> do
                    when (qClient q /= pClient ping) $ error "client doesn't match the ping"
                    server <- return $ addQuestion now q server
                    return $ Right (server, Just q)
                Update (s,ps) -> do
                    (s2, answer) <- runUpdate s ps
                    case s2 of
                        Nothing -> do
                            ovenNotify oven [a | p <- Nothing : map Just (snd $ target server), a <- Map.findWithDefault [] p $ authors server]
                                "Failed to update, pretty serious"
                            return $ Right (addUpdate now answer Nothing (s,ps) server, Nothing)
                        Just s2 -> do
                            ovenNotify oven [a | p <- ps, a <- Map.findWithDefault [] (Just p) $ authors server]
                                "Your patch just made it in"
                            addDelayCache (extra server) (Left s2) $ patchExtra s2 Nothing
                            return $ Left $ addUpdate now answer (Just s2) (s,ps) server
                Reject p t -> do
                    ovenNotify oven (Map.findWithDefault [] (Just p) (authors server)) $ unlines
                        ["Your patch " ++ show p ++ " got rejected","Failure in test " ++ show t]
                    return $ Left server{target=second (delete p) $ target server}
    where
        dull s = return (s,Nothing)
