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
import Development.Bake.Server.History
import Development.Bake.Server.Memory
import Development.Bake.Server.Store
import Control.Applicative
import Control.DeepSeq
import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Control.Monad.Extra
import System.Console.CmdArgs.Verbosity
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS
import Prelude


startServer :: (Stringy state, Stringy patch, Stringy test)
            => Port -> FilePath -> Author -> String -> Double -> String -> Oven state patch test -> IO ()
startServer port datadir author name timeout admin (concrete -> (prettys, oven)) = do
    extra <- newWorker
    var <- newCVar =<< initialise oven author extra

    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            now <- getCurrentTime
            let prune = expire (addSeconds (negate timeout) now)
            res <-
                if null inputURL then do
                    -- prune but don't save, will reprune on the next ping
                    fmap OutputHTML $ web prettys admin inputArgs . prune =<< readCVar var
                else if ["history"] `isPrefixOf` inputURL then
                    fmap (OutputString . BS.unpack) readHistory 
                else if ["html"] `isPrefixOf` inputURL then
                    return $ OutputFile $ datadir </> "html" </> last inputURL
                else if ["api"] `isPrefixOf` inputURL then
                    (case messageFromInput i{inputURL = drop 1 inputURL} of
                        Left e -> return $ OutputError e
                        Right v -> do
                            evaluate $ rnf v
                            fmap questionToOutput $ modifyCVar var $ \s -> do
                                case v of
                                    AddPatch _ p -> extra $ do
                                        res <- patchExtra (fst $ active s) $ Just p
                                        storeExtraAdd (store s) (Right p) res
                                    _ -> return ()
                                (s2,q) <- recordIO $ (["brain"],) <$> prod oven (prune s) v
                                when (fst (active s2) /= fst (active s)) $ extra $ do
                                    res <- patchExtra (fst $ active s2) Nothing
                                    storeExtraAdd (store s2) (Left $ fst $ active s2) res
                                return (s2,q)
                    )
                else
                    return OutputMissing
            evaluate $ force res


initialise :: Oven State Patch Test -> String -> Worker -> IO Memory
initialise oven author extra = do
    now <- getCurrentTime
    putStrLn "Initialising server, computing initial state..."
    (res, answer) <- runInit
    when (isNothing res) $
        ovenNotify oven [author] "Failed to initialise, pretty serious"
    let state0 = fromMaybe stateFailure res
    putStrLn $ "Initial state: " ++ maybe "!FAILURE!" fromState res
    addHistory [(HRestart, toPatch "")]
    store <- newStore False "bake-store"
    when (isJust res) $ do
        extra $ storeExtraAdd store (Left state0) =<< patchExtra state0 Nothing
    mem <- newMemory store (state0, answer)
    return $ mem
        {authors=[author]
--        ,updates=[Update now answer state0 []]
        ,fatal=["Failed to initialise" | isNothing res]
        }


-- | Get information about a patch
patchExtra :: State -> Maybe Patch -> IO (T.Text, TL.Text)
patchExtra s p = do
    (ex,ans) <- runExtra s p
    let failSummary = T.pack $ renderHTML $ i_ $ str_ "Error when computing patch information"
    let failDetail = TL.pack $ renderHTML $ pre_ $ str_ $ TL.unpack $ aStdout ans
    return $ fromMaybe (failSummary, failDetail) ex
