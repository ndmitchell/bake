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
import qualified Data.Map as Map
import Prelude


startServer :: Port -> FilePath -> Author -> String -> Double -> Oven state patch test -> IO ()
startServer port datadir author name timeout (validate . concrete -> oven) = do
    do
        dir <- getCurrentDirectory
        strInit (dir </> "bake-string") (25 * 1024 * 1024) -- use at most 25Mb for strings
    extra <- newDelayCache
    var <- do
        now <- getCurrentTime
        putStrLn "Initialising server, computing initial state..."
        (res, answer) <- runInit
        when (isNothing res) $
            ovenNotify oven [author] "Failed to initialise, pretty serious"
        let state0 = fromMaybe stateFailure res
        putStrLn $ "Initial state: " ++ maybe "!FAILURE!" fromState res
        when (isJust res) $ addDelayCache extra (Left state0) $ patchExtra state0 Nothing
        newCVar $ new
            {active=(state0,[])
            ,authors=Map.fromList [(Nothing,[author])]
            ,updates=[Update now answer state0 []]
            ,fatal=["Failed to initialise" | isNothing res]
            }

    server port $ \i@Input{..} -> do
        whenLoud $ print i
        handle_ (fmap OutputError . showException) $ do
            now <- getCurrentTime
            let prune = id -- expire (addSeconds (negate timeout) now)
            res <-
                if null inputURL then do
                    -- prune but don't save, will reprune on the next ping
                    fmap OutputHTML $ web oven extra inputArgs . prune =<< readCVar var
                else if ["html"] `isPrefixOf` inputURL then
                    return $ OutputFile $ datadir </> "html" </> last inputURL
                else if ["api"] `isPrefixOf` inputURL then
                    (case messageFromInput i{inputURL = drop 1 inputURL} of
                        Left e -> return $ OutputError e
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


-- | Get information about a patch
patchExtra :: State -> Maybe Patch -> IO (Str, Str)
patchExtra s p = do
    (ex,ans) <- runExtra s p
    let failSummary = renderHTML $ i_ $ str_ "Error when computing patch information"
    let failDetail = renderHTML $ pre_ $ str_ $ strUnpack $ aStdout ans
    return $ fromMaybe (strPack failSummary, strPack failDetail) ex
