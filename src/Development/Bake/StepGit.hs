{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.StepGit(
    ovenStepGit
    ) where

import Development.Bake.Core.Type
import Development.Bake.Git
import Development.Shake.Command
import Development.Shake.FilePath
import Control.Exception.Extra
import Control.Monad.Extra
import System.Directory.Extra
import General.Extra
import Data.Maybe
import Data.List.Extra
import System.IO.Extra
import System.IO.Unsafe


-- | Oven creation for modules using git with the step strategy.
ovenStepGit
    :: IO [FilePath] -- ^ Function that does a compile and returns the pieces that should be available at test time
    -> String -- ^ Git repo you are using
    -> String -- ^ Branch used as the initial starting point
    -> Maybe FilePath -- ^ Path under which the git will be checked out
    -> Oven () () test -- ^ Normal oven
    -> Oven SHA1 SHA1 test
ovenStepGit act repo branch path o = o
    {ovenInit = gitInit repo branch
    ,ovenUpdate = stepUpdate
    ,ovenPrepare = \s ps -> do stepPrepare s ps; ovenPrepare o () $ map (const ()) ps
    ,ovenSupersede = \_ _ -> False
    ,ovenPatchExtra = stepExtra
    }
    where
        -- use a different failure name each run, so failures don't get persisted
        failure = unsafePerformIO $ do
            t <- getCurrentTime
            return $ "failure-" ++ showUTCTime "%Y-%m-%dT%H-%M-%S%Q" t <.> "txt"
        root = createDir "../bake-step-git" [repo,branch]

        gitEnsure = do
            root <- root
            let git = root </> fromMaybe "repo" path
            createDirectoryIfMissing True git
            withFileLock (root </> ".bake-lock") $ do
                ready <- doesFileExist $ git </> ".git/HEAD"
                if ready then do
                    -- for some reason git sometimes times out, not sure why
                    -- hopefully this will help track it down
                    time_ $ cmd (Cwd git) (Timeout $ 15*60) "git fetch"
                    -- stops us creating lots of garbage in the reflog, which slows everything down
                    -- time_ $ cmd (Cwd git) "git reflog expire --all --expire=all --expire-unreachable=all"
                 else do
                    time_ $ cmd (Cwd git) "git clone" [repo] "."
                    time_ $ cmd (Cwd git) "git config user.email" ["https://github.com/ndmitchell/bake"]
                    time_ $ cmd (Cwd git) "git config user.name" ["Bake Continuous Integration"]
            return git

        gitSetState git s = do
            time_ $ cmd (Cwd git) "git checkout --force -B" [branch] [fromSHA1 s]

        gitApplyPatch git p = do
            time_ $ cmd (Cwd git) "git merge" [fromSHA1 p]

        stepExtra s p = do
            root <- root
            let (sh,a1) = splitAt 2 $ fromSHA1 $ fromMaybe s p
            unlessM (doesFileExist $ root </> fromMaybe "repo" path </> ".git/objects" </> sh </> a1) $ do
                void gitEnsure
            gitPatchExtra s p $ root </> fromMaybe "repo" path

        stepUpdate s ps = do
            root <- root
            git <- gitEnsure
            withFileLock (root </> ".bake-lock") $ do
                gitSetState git s
                forM_ ps $ gitApplyPatch git
                Stdout x <- time $ cmd (Cwd git) "git rev-parse" [branch]
                time_ $ cmd (Cwd git) "git push" [repo] [branch ++ ":" ++ branch]
                return $ sha1 $ trim x

        stepPrepare s ps = do
            root <- root
            dir <- createDir (root </> "../bake-step-point") $ map fromSHA1 $ s : ps
            unlessM (doesFileExist $ dir </> "result.tar") $ do
                git <- gitEnsure
                withFileLock (root </> ".bake-lock") $ do
                    gitSetState git s
                    forM_ (inits ps) $ \ps -> do
                        when (ps /= []) $ do
                            gitApplyPatch git $ last ps
                        dir <- createDir (root </> "../bake-step-point") $ map fromSHA1 $ s : ps
                        unlessM (doesFileExist $ dir </> "result.tar") $ do
                            whenM (doesFileExist $ dir </> failure) $ do
                                hPutStrLn stderr "failure found"
                                fail =<< readFile' (dir </> failure)
                            res <- withCurrentDirectory git (timed "stepPrepare user action" act) `catch_` \e -> do
                                writeFile (dir </> failure) =<< showException e
                                throwIO e
                            time_ $ cmd "tar -cf" [toStandard $ dir </> "result.tar"] "-C" [toStandard git] res

            createDirectoryIfMissing True $ fromMaybe "." path
            time_ $ cmd "tar -xf" [toStandard $ dir </> "result.tar"] "-C" [toStandard $ fromMaybe "." path]
