{-# LANGUAGE RecordWildCards, NamedFieldPuns, TupleSections, ViewPatterns #-}

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
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Paths_bake
import Prelude


startServer :: (Stringy state, Stringy patch, Stringy test)
            => Port -> [Author] -> Seconds -> String -> Bool -> Oven state patch test -> IO ()
startServer port authors timeout admin fake (concrete -> (prettys, oven)) = do
    extra <- newWorker
    var <- newCVar =<< if fake then initialiseFake oven prettys else initialise oven prettys authors extra

    forkSlave $ forever $ do
        sleep timeout
        now <- getCurrentTime
        let prune = expire (addSeconds (negate timeout) now)
        modifyCVar_ var $ \s -> do
            let s2 = prune s
            bad <- clientChange s s2
            return $ bad s2

    putStrLn $ "Started server on port " ++ show port
    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            now <- getCurrentTime
            let prune = expire (addSeconds (negate timeout) now)
            res <-
                if null inputURL then do
                    -- prune but don't save, will reprune on the next ping
                    fmap OutputHTML $ web admin inputArgs . prune =<< readCVar var
                else if ["html"] `isPrefixOf` inputURL then do
                    datadir <- getDataDir
                    return $ OutputFile $ datadir </> "html" </> last inputURL

                else if inputURL == ["dump"] then do
                    mem <- readCVar var
                    storeSave "temp.sqlite" $ store mem
                    return $ OutputFile "temp.sqlite"

                else if inputURL == ["alive"] then do
                    Memory{store} <- readCVar var
                    let xs = sortOn (paQueued . storePatch store) $ Set.toList $ storeAlive store
                    return $ OutputString $ unlines $ map fromPatch xs

                else if inputURL == ["active"] then do
                    Memory{active} <- readCVar var
                    return $ OutputString $ unlines $ map fromPatch $ snd active

                else if inputURL == ["state"] then do
                    Memory{active} <- readCVar var
                    return $ OutputString $ unlines [fromState $ fst active]

                else if inputURL == ["skip"] then do
                    Memory{store} <- readCVar var
                    return $ OutputString $ unlines $ map fromTest $ Map.keys $ storeSkip store

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
                                (s2,q) <- recordIO $ (["brain",lower $ fst $ word1 $ show v],) <$> prod (prune s) v
                                when (fst (active s2) /= fst (active s)) $ extra $ do
                                    res <- patchExtra (fst $ active s2) Nothing
                                    storeExtraAdd (store s2) (Left $ fst $ active s2) res
                                bad <- clientChange s s2
                                when (fatal s == [] && fatal s2 /= []) $ do
                                    void $ notifyAdmins s2 "Fatal error" $ pre_ $ summary $ head $ fatal s2
                                return (bad s2,q)
                            return $ case res of
                                Just (Left e) -> OutputError e
                                Just (Right q) -> questionToOutput $ Just q
                                Nothing -> questionToOutput Nothing
                else
                    return OutputMissing
            evaluate $ force res


clientChange :: Memory -> Memory -> IO (Memory -> Memory)
clientChange s1 s2 = do
    let before = Map.keysSet $ clients s1
    let after  = Map.keysSet $ clients s2
    let f msg xs = sequence [notifyAdmins s2 (msg ++ ": " ++ fromClient x) $ str_ "" | x <- Set.toList xs]
    a <- f "Client added" $ after `Set.difference` before
    b <- f "Client timed out" $ before `Set.difference` after
    return $ foldr (.) id $ a ++ b


initialiseFake :: Oven State Patch Test -> Prettys -> IO Memory
initialiseFake oven prettys = do
    store <- newStore False "bake-store"
    mem <- newMemory oven prettys store (stateFailure, Answer (TL.pack "Initial state created by view mode") Nothing [] False)
    return mem{fatal = ["View mode, database is read-only"]}

initialise :: Oven State Patch Test -> Prettys -> [Author] -> Worker -> IO Memory
initialise oven prettys admins extra = do
    now <- getCurrentTime
    putStrLn "Initialising server, computing initial state..."
    (res, answer) <- runInit
    let state0 = fromMaybe stateFailure res
    putStrLn $ "Initial state: " ++ maybe "!FAILURE!" fromState res
    store <- newStore False "bake-store"
    when (isJust res) $ do
        extra $ storeExtraAdd store (Left state0) =<< patchExtra state0 Nothing
    mem <- newMemory oven prettys store (state0, answer)
    mem <- return mem{admins = admins ,fatal = ["Failed to initialise, " ++ TL.unpack (aStdout answer) | isNothing res]}

    bad <- if isJust res then notifyAdmins mem "Starting" $ str_ "Server starting" else
        notifyAdmins mem "Fatal error during initialise" $
            str_ "Failed to initialise" <> br_ <> pre_ (summary $ TL.unpack $ aStdout answer)
    return $ bad mem


-- | Get information about a patch
patchExtra :: State -> Maybe Patch -> IO (T.Text, TL.Text)
patchExtra s p = do
    (ex,ans) <- runExtra s p
    let failSummary = T.pack $ renderHTML $ i_ $ str_ "Error when computing patch information"
    let failDetail = TL.pack $ renderHTML $ pre_ $ str_ $ TL.unpack $ aStdout ans
    return $ fromMaybe (failSummary, failDetail) ex
