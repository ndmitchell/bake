{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Start(
    startServer
    ) where

import Development.Bake.Core.Type
import General.Web
import General.HTML
import Development.Bake.Core.Message
import Development.Bake.Core.Run
import General.Extra
import Development.Bake.Server.Brain
import Development.Bake.Server.Web
import Development.Bake.Server.Stats
import Development.Bake.Server.Memory
import Development.Bake.Server.Store
import Control.Applicative
import System.Time.Extra
import Control.DeepSeq
import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Control.Monad.Extra
import System.Console.CmdArgs.Verbosity
import System.FilePath
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Paths_bake
import Prelude


startServer :: (Stringy state, Stringy patch, Stringy test)
            => Port -> [Author] -> Seconds -> String -> Bool -> Oven state patch test -> IO ()
startServer port authors timeout admin fake (concrete -> (prettys, oven)) = do
    extra <- newWorker
    var <- newCVar =<< if fake then initialiseFake else initialise oven authors extra

    forkSlave $ forever $ do
        sleep timeout
        now <- getCurrentTime
        let prune = expire (addSeconds (negate timeout) now)
        modifyCVar_ var $ \s -> do
            let s2 = prune s
            s2 <- if Map.keysSet (clients s) == Map.keysSet (clients s2) then return s2 else do
                res <- try_ $ ovenNotify oven (admins s2) $ unlines
                    ["Set of clients has changed"
                    ,"Was: " ++ unwords (map fromClient $ Map.keys $ clients s)
                    ,"Now: " ++ unwords (map fromClient $ Map.keys $ clients s2)]
                return s2{fatal = ["Error when notifying, " ++ show e | Left e <- [res]] ++ fatal s2}
            return s2

    putStrLn $ "Started server on port " ++ show port
    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            now <- getCurrentTime
            let prune = expire (addSeconds (negate timeout) now)
            res <-
                if null inputURL then do
                    -- prune but don't save, will reprune on the next ping
                    fmap OutputHTML $ web prettys admin inputArgs . prune =<< readCVar var
                else if ["html"] `isPrefixOf` inputURL then do
                    datadir <- getDataDir
                    return $ OutputFile $ datadir </> "html" </> last inputURL
                else if inputURL == ["dump"] then do
                    mem <- readCVar var
                    storeSave "temp.sqlite" $ store mem
                    return $ OutputFile "temp.sqlite"
                else if ["api"] `isPrefixOf` inputURL then
                    case messageFromInput i{inputURL = drop 1 inputURL} of
                        Left e -> return $ OutputError e
                        Right v -> do
                            evaluate $ rnf v
                            res <- modifyCVar var $ \s -> do
                                case v of
                                    AddPatch _ p -> extra $ do
                                        res <- patchExtra (fst $ active s) $ Just p
                                        storeExtraAdd (store s) (Right p) res
                                    _ -> return ()
                                (s2,q) <- recordIO $ (["brain"],) <$> prod oven (prune s) v
                                when (fst (active s2) /= fst (active s)) $ extra $ do
                                    res <- patchExtra (fst $ active s2) Nothing
                                    storeExtraAdd (store s2) (Left $ fst $ active s2) res
                                s2 <- if Map.keysSet (clients s) == Map.keysSet (clients s2) then return s2 else do
                                    res <- try_ $ ovenNotify oven (admins s2) $ unlines
                                        ["Set of clients has changed"
                                        ,"Was: " ++ unwords (map fromClient $ Map.keys $ clients s)
                                        ,"Now: " ++ unwords (map fromClient $ Map.keys $ clients s2)]
                                    return s2{fatal = ["Error when notifying, " ++ show e | Left e <- [res]] ++ fatal s2}
                                when (fatal s == [] && fatal s2 /= []) $
                                    void $ try_ $ ovenNotify oven (admins s2) $ "Fatal error\n" ++ head (fatal s2)
                                return (s2,q)
                            return $ case res of
                                Just (Left e) -> OutputError e
                                Just (Right q) -> questionToOutput $ Just q
                                Nothing -> questionToOutput Nothing
                else
                    return OutputMissing
            evaluate $ force res


initialiseFake :: IO Memory
initialiseFake = do
    store <- newStore False "bake-store"
    mem <- newMemory store (toState "", Answer (TL.pack "Initial state created by view mode") Nothing [] False)
    return mem{fatal = ["View mode, database is read-only"]}

initialise :: Oven State Patch Test -> [Author] -> Worker -> IO Memory
initialise oven admins extra = do
    now <- getCurrentTime
    putStrLn "Initialising server, computing initial state..."
    (res, answer) <- runInit
    when (isNothing res) $
        void $ try_ $ ovenNotify oven admins "Failed to initialise, pretty serious"
    let state0 = fromMaybe stateFailure res
    putStrLn $ "Initial state: " ++ maybe "!FAILURE!" fromState res
    store <- newStore False "bake-store"
    when (isJust res) $ do
        extra $ storeExtraAdd store (Left state0) =<< patchExtra state0 Nothing
    mem <- newMemory store (state0, answer)

    email <- if isNothing res then return $ Right () else
        try_ $ ovenNotify oven admins "Server starting up"

    return $ mem
        {admins = admins
        ,fatal = ["Failed to initialise, " ++ TL.unpack (aStdout answer) | isNothing res] ++
                 ["Failed to notify, " ++ show e | Left e <- [email]]
        }


-- | Get information about a patch
patchExtra :: State -> Maybe Patch -> IO (T.Text, TL.Text)
patchExtra s p = do
    (ex,ans) <- runExtra s p
    let failSummary = T.pack $ renderHTML $ i_ $ str_ "Error when computing patch information"
    let failDetail = TL.pack $ renderHTML $ pre_ $ str_ $ TL.unpack $ aStdout ans
    return $ fromMaybe (failSummary, failDetail) ex
