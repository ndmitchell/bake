{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.Step(
    ovenStepGit, stepGet
    ) where

import Development.Bake.Core.Type
import Development.Bake.Git
import Development.Shake.Command
import Development.Shake.FilePath
import Control.Exception.Extra
import Control.Monad.Extra
import Control.Applicative
import System.Directory.Extra
import General.Extra
import Data.Maybe
import Data.List.Extra
import System.IO.Extra
import System.IO.Unsafe


ovenStepGit :: IO [FilePath] -> String -> String -> Maybe FilePath -> Oven () () test -> Oven SHA1 SHA1 test
ovenStepGit act repo branch (fromMaybe "repo" -> path) o = o
    {ovenUpdateState = ovenUpdateState $ ovenGit repo branch (Just path) o
    ,ovenPrepare = \s ps -> do stepPrepare s ps; ovenPrepare o () $ map (const ()) ps
    ,ovenPatchExtra = stepExtra
    ,ovenStringyState = stringySHA1
    ,ovenStringyPatch = stringySHA1
    }
    where
        -- use a different failure name each run, so failures don't get persisted
        failure = unsafePerformIO $ do
            t <- getCurrentTime
            return $ "failure-" ++ showUTCTime "%Y-%m-%dT%H-%M-%S%Q" t <.> "txt"
        root = createDir "../bake-step-git" [repo,branch]

        gitEnsure = do
            root <- root
            let git = root </> path
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

        stepExtra s p = do
            root <- root
            let (sh,a1) = splitAt 2 $ fromSHA1 $ fromMaybe s p
            unlessM (doesFileExist $ root </> path </> ".git/objects" </> sh </> a1) $ do
                void $ gitEnsure
            gitPatchExtra s p $ root </> path

        stepPrepare s ps = do
            logEntry "stepPrepare"
            root <- root
            dir <- createDir (root </> ".bake-point") $ map fromSHA1 $ s : ps
            unlessM (doesFileExist $ dir </> "result.txt") $ do
                git <- gitEnsure
                logEntry "stepPrepare after gitEnsure"
                withFileLock (root </> ".bake-lock") $ do
                    logEntry "stepPrepare git initialise"
                    unit $ cmd (Cwd git) "git checkout" [branch]
                    unit $ cmd (Cwd git) "git reset --hard" ["origin/" ++ branch]
                    Stdout x <- cmd (Cwd git) "git rev-parse HEAD"
                    when (trim x /= fromSHA1 s) $ error $
                        "The branch " ++ branch ++ " changed SHA1 independently of bake.\n" ++
                        "Expected value: " ++ fromSHA1 s ++ "\n" ++
                        "But has become: " ++ trim x
                    forM_ (inits ps) $ \ps -> do
                        when (ps /= []) $ do
                            unit $ cmd (Cwd git) "git merge" (fromSHA1 $ last ps)
                        logEntry "stepPrepare after merge"
                        dir <- createDir (root </> ".bake-point") $ map fromSHA1 $ s : ps
                        unlessM (doesFileExist $ dir </> "result.txt") $ do
                            whenM (doesFileExist $ dir </> failure) $ do
                                hPutStrLn stderr "failure found"
                                fail =<< readFile' (dir </> failure)
                            res <- withCurrentDirectory git (timed "stepPrepare user action" act) `catch_` \e -> do
                                writeFile (dir </> failure) =<< showException e
                                throwIO e
                            xs <- forM (zip [0..] res) $ \(i,out) -> do
                                logEntry "stepPrepare before tar"
                                let tar = dir </> show i <.> "tar"
                                --tarrel <- makeRelativeEx (git </> out) tar
                                --print ("running tar", dir, tar, git </> out, tarrel)
                                timed "tar create" $ unit $ cmd "tar -cf" [toStandard tar] "-C" [git </> out] "."
                                md5 <- timed "running md5" $ fst . word1 . fromStdout <$> cmd "md5sum" [toStandard tar]
                                let out = root </> ".bake-" ++ show i ++ "-" ++ md5 <.> "tar"
                                ifM (doesFileExist out) (removeFile tar) (renameFile tar out)
                                logEntry "stepPrepare after tar"
                                return out
                            writeFile (dir </> "result.txt") $ unlines xs
            src <- lines <$> readFile' (dir </> "result.txt")
            logEntry "stepPrepare before extract"
            dirs <- forM src $ \src -> do
                createDirectoryIfMissing True $ dropExtension src
                timed "tar extract" $ unit $ cmd "tar -xf" [src] "-C" [dropExtension src]
            logEntry "stepPrepare after extract"
            writeFile ".bake-step" $ unlines $ map (toNative . dropExtension) src


stepGet :: IO [FilePath]
stepGet = lines <$> readFile' ".bake-step"
