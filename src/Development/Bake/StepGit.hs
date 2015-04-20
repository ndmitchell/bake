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


ovenStepGit :: IO [FilePath] -> String -> String -> Maybe FilePath -> Oven () () test -> Oven SHA1 SHA1 test
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
                if ready then
                    timed "git fetch for mirror" $ unit $ cmd (Cwd git) "git fetch"
                 else do
                    timed "git clone for mirror" $ unit $ cmd (Cwd git) "git clone" [repo] "."
                    unit $ cmd (Cwd git) "git config user.email" ["https://github.com/ndmitchell/bake"]
                    unit $ cmd (Cwd git) "git config user.name" ["Bake Continuous Integration"]
            return git

        gitSetState git s = do
            unit $ cmd (Cwd git) "git reset --merge"
            unit $ cmd (Cwd git) "git checkout" [branch]
            unit $ cmd (Cwd git) "git reset --hard" ["origin/" ++ branch]
            Stdout x <- cmd (Cwd git) "git rev-parse HEAD"
            when (trim x /= fromSHA1 s) $ error $
                "The branch " ++ branch ++ " changed SHA1 independently of bake.\n" ++
                "Expected value: " ++ fromSHA1 s ++ "\n" ++
                "But has become: " ++ trim x

        gitApplyPatch git p = do
            unit $ cmd (Cwd git) "git merge" [fromSHA1 p]

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
                Stdout x <- cmd (Cwd git) "git rev-parse" [branch]
                unit $ cmd (Cwd git) "git push" [repo] [branch ++ ":" ++ branch]
                return $ sha1 $ trim x

        stepPrepare s ps = do
            logEntry "stepPrepare"
            root <- root
            dir <- createDir (root </> "../bake-step-point") $ map fromSHA1 $ s : ps
            unlessM (doesFileExist $ dir </> "result.tar") $ do
                git <- gitEnsure
                logEntry "stepPrepare after gitEnsure"
                withFileLock (root </> ".bake-lock") $ do
                    logEntry "stepPrepare git initialise"
                    gitSetState git s
                    forM_ (inits ps) $ \ps -> do
                        when (ps /= []) $ do
                            gitApplyPatch git $ last ps
                        logEntry "stepPrepare after merge"
                        dir <- createDir (root </> "../bake-step-point") $ map fromSHA1 $ s : ps
                        unlessM (doesFileExist $ dir </> "result.tar") $ do
                            whenM (doesFileExist $ dir </> failure) $ do
                                hPutStrLn stderr "failure found"
                                fail =<< readFile' (dir </> failure)
                            res <- withCurrentDirectory git (timed "stepPrepare user action" act) `catch_` \e -> do
                                writeFile (dir </> failure) =<< showException e
                                throwIO e
                            logEntry "stepPrepare before tar"
                            timed "tar create" $ unit $ cmd "tar -cf" [toStandard $ dir </> "result.tar"] "-C" [toStandard git] res
                            logEntry "stepPrepare after tar"

            logEntry "stepPrepare before extract"
            createDirectoryIfMissing True $ fromMaybe "." path
            timed "tar extract" $ unit $ cmd "tar -xf" [toStandard $ dir </> "result.tar"] "-C" [toStandard $ fromMaybe "." path]
            logEntry "stepPrepare after extract"
