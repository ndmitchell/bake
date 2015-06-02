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
import Development.Bake.Server.Brain
import Development.Bake.Server.Web
import Development.Bake.Server.Stats
import Development.Bake.Server.History
import Development.Bake.Server.Memory
import Development.Bake.Server.Store
import General.DelayCache
import Control.Applicative
import Control.DeepSeq
import Control.Exception.Extra
import Data.List.Extra
import Data.Maybe
import Control.Monad.Extra
import System.Directory
import System.Console.CmdArgs.Verbosity
import System.FilePath
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS
import Prelude


startServer :: (Stringy state, Stringy patch, Stringy test)
            => Port -> FilePath -> Author -> String -> Double -> String -> Oven state patch test -> IO ()
startServer port datadir author name timeout admin (concrete -> (prettys, oven)) = do
    do
        dir <- getCurrentDirectory
        strInit (dir </> "bake-string") (25 * 1024 * 1024) -- use at most 25Mb for strings
    extra <- newDelayCache
    var <- newCVar =<< initialise oven author extra

    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            now <- getCurrentTime
            let prune = expire (addSeconds (negate timeout) now)
            res <-
                if null inputURL then do
                    -- prune but don't save, will reprune on the next ping
                    fmap OutputHTML $ web extra prettys admin inputArgs . prune =<< readCVar var
                else if ["history"] `isPrefixOf` inputURL then
                    fmap (OutputString . BS.unpack) readHistory 
                else if ["html"] `isPrefixOf` inputURL then
                    return $ OutputFile $ datadir </> "html" </> last inputURL
                else if ["api"] `isPrefixOf` inputURL then
                    (case messageFromInput i{inputURL = drop 1 inputURL} of
                        Left e -> return $ OutputError e
                        Right (Reinit v) -> do
                            modifyCVar_ var $ const $ initialise oven author extra
                            return $ questionToOutput Nothing
                        Right v -> do
                            evaluate $ rnf v
                            fmap questionToOutput $ modifyCVar var $ \s -> do
                                case v of
                                    AddPatch _ p -> addDelayCache extra (Right p) $ patchExtra (fst $ active s) $ Just p
                                    _ -> return ()
                                recordIO $ (["brain"],) <$> prod oven (prune s) v
                    )
                else
                    return OutputMissing
            evaluate $ force res


initialise :: Oven State Patch Test -> String -> DelayCache (Either State Patch) (Str, Str) -> IO Memory
initialise oven author extra = do
    now <- getCurrentTime
    putStrLn "Initialising server, computing initial state..."
    (res, answer) <- runInit
    when (isNothing res) $
        ovenNotify oven [author] "Failed to initialise, pretty serious"
    let state0 = fromMaybe stateFailure res
    putStrLn $ "Initial state: " ++ maybe "!FAILURE!" fromState res
    when (isJust res) $ addDelayCache extra (Left state0) $ patchExtra state0 Nothing
    addHistory [(HRestart, Patch "")]
    store <- newStore False "bake-store"
    mem <- newMemory store state0
    return $ mem
        {authors=[author]
--        ,updates=[Update now answer state0 []]
        ,fatal=["Failed to initialise" | isNothing res]
        }


-- | Get information about a patch
patchExtra :: State -> Maybe Patch -> IO (Str, Str)
patchExtra s p = do
    (ex,ans) <- runExtra s p
    let failSummary = renderHTML $ i_ $ str_ "Error when computing patch information"
    let failDetail = renderHTML $ pre_ $ str_ $ TL.unpack $ aStdout ans
    return $ fromMaybe (strPack failSummary, strPack failDetail) ex
