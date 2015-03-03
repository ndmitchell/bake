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


ovenStepGit :: IO [FilePath] -> String -> String -> Maybe FilePath -> Oven () () test -> Oven SHA1 SHA1 test
ovenStepGit act repo branch (fromMaybe "repo" -> path) o = o
    {ovenUpdateState = ovenUpdateState $ ovenGit repo branch (Just path) o
    ,ovenPrepare = \s ps -> do stepPrepare s ps; ovenPrepare o () $ map (const ()) ps
    ,ovenPatchExtra = stepExtra
    ,ovenStringyState = stringySHA1
    ,ovenStringyPatch = stringySHA1
    }
    where
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
            root <- root
            dir <- createDir (root </> ".bake-point") $ map fromSHA1 $ s : ps
            unlessM (doesFileExist $ dir </> "result.txt") $ do
                git <- gitEnsure
                withFileLock (root </> ".bake-lock") $ do
                    unit $ cmd (Cwd git) "git reset --hard" ["origin/" ++ branch]
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
                        dir <- createDir (root </> ".bake-point") $ map fromSHA1 $ s : ps
                        unlessM (doesFileExist $ dir </> "result.txt") $ do
                            whenM (doesFileExist $ dir </> "failure.txt") $ do
                                hPutStrLn stderr "failure found"
                                fail =<< readFile' (dir </> "failure.txt")
                            res <- withCurrentDirectory git $ act `catch_` \e -> do
                                writeFile (dir </> "failure.txt") =<< showException e
                                throwIO e
                            xs <- forM (zip [0..] res) $ \(i,out) -> do
                                dir <- canonicalizePath dir
                                let tar = dir </> show i <.> "tar"
                                unit $ cmd (Cwd $ git </> out) "tar -cf" [toStandard $ ".." </> tar] ["."]
                                md5 <- fst . word1 . fromStdout <$> cmd "md5sum" [toStandard tar]
                                let out = root </> ".bake-" ++ show i ++ "-" ++ md5 <.> "tar"
                                ifM (doesFileExist out) (removeFile tar) (renameFile tar out)
                                return out
                            writeFile (dir </> "result.txt") $ unlines xs
            src <- lines <$> readFile' (dir </> "result.txt")
            dirs <- forM src $ \src -> do
                createDirectoryIfMissing True $ dropExtension src
                unit $ cmd "tar -xf" [src] "-C" [dropExtension src]
            writeFile ".bake-step" $ unlines $ map (toNative . dropExtension) src


stepGet :: IO [FilePath]
stepGet = lines <$> readFile' ".bake-step"
