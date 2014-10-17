{-# LANGUAGE ViewPatterns #-}

module Development.Bake.Git(
    SHA1, ovenGit,
    ) where

import Development.Bake.Type
import Development.Bake.Util
import Development.Shake.Command
import Control.Monad.Extra
import Data.List.Extra
import Development.Bake.Format
import System.Directory.Extra
import System.FilePath
import Data.Maybe


newtype SHA1 = SHA1 {fromSHA1 :: String} deriving (Show,Eq)

sha1 :: String -> SHA1
sha1 x | length x /= 40 = error $ "SHA1 for Git must be 40 characters long, got " ++ show x
       | not $ all (`elem` "0123456789abcdef") x = error $ "SHA1 for Git must be all lower case hex, got " ++ show x 
       | otherwise = SHA1 x

stringySHA1 :: Stringy SHA1
stringySHA1 = Stringy
    {stringyTo = fromSHA1
    ,stringyFrom = sha1
    ,stringyPretty = take 7 . fromSHA1
    }


-- | Modify an 'Oven' to work with the Git version control system.
--   Requires the name of the repo (e.g. @https:\/\/github.com\/ndmitchell\/bake.git@)
--   and the name of a branch (e.g. @master@). You can optionally give a path fragment
--   which is used to clone into.
ovenGit :: String -> String -> Maybe FilePath -> Oven () () test -> Oven SHA1 SHA1 test
ovenGit repo branch (fromMaybe "." -> path) o = o
    {ovenUpdateState = gitUpdateState
    ,ovenPrepare = \s ps -> do gitCheckout s ps; ovenPrepare o () $ map (const ()) ps
    ,ovenPatchExtra = gitPatchExtra
    ,ovenStringyState = stringySHA1
    ,ovenStringyPatch = stringySHA1
    }
    where
        traced msg act = do
            putStrLn $ "% GIT: Begin " ++ msg
            res <- act
            putStrLn $ "% GIT: Finish " ++ msg
            return res

        gitSafe dir = do
            unit $ cmd (Cwd dir) "git config user.email" ["https://github.com/ndmitchell/bake"]
            unit $ cmd (Cwd dir) "git config user.name" ["Bake Continuous Integration"]

        -- initialise the mirror, or make it up to date
        gitInitMirror = traced "gitInitMirror" $ do
            mirror <- createDir "../bake-git" [repo]
            -- see http://blog.plataformatec.com.br/2013/05/how-to-properly-mirror-a-git-repository/
            ready <- doesFileExist $ mirror </> "HEAD"
            if ready then
                unit $ cmd (Cwd mirror) "git fetch --prune"
             else do
                unit $ cmd (Cwd mirror) "git clone --mirror" [repo] "."
                gitSafe mirror
            return mirror

        gitUpdateState Nothing = traced "gitUpdateState Nothing" $ do
            mirror <- gitInitMirror
            Stdout hash <- cmd (Cwd mirror) "git rev-parse" [branch]
            case words hash of
                [] -> error "Couldn't find branch"
                x:xs -> return $ sha1 $ strip x

        gitUpdateState (Just (s, ps)) = traced "gitUpdateState Just" $ do
            gitCheckout s ps
            Stdout x <- cmd (Cwd path) "git rev-parse" [branch]
            unit $ cmd (Cwd path) "git push" [repo] [branch ++ ":" ++ branch]
            return $ sha1 $ strip x

        gitCheckout s ps = traced "gitCheckout" $ do
            createDirectoryIfMissing True path
            mirror <- gitInitMirror
            b <- doesDirectoryExist $ path </>".git"
            if b then
                unit $ cmd (Cwd path) "git pull origin"
             else do
                unit $ cmd (Cwd path) "git clone" [(if path == "." then "" else "../") ++ mirror] "." ["--branch",branch]
                gitSafe path
            unit $ cmd (Cwd path) "git checkout" [branch]
            unit $ cmd (Cwd path) "git reset --hard" ["origin/" ++ branch]
            Stdout x <- cmd (Cwd path) "git rev-parse HEAD"
            when (strip x /= fromSHA1 s) $ error "Branch changed while running"
            forM_ ps $ \p ->
                unit $ cmd (Cwd path) "git merge" (fromSHA1 p)

        gitPatchExtra s Nothing = traced "gitPatchExtra Nothing" $ do
            mirror <- gitInitMirror
            Stdout full <- cmd (Cwd mirror) "git log -n3" [fromSHA1 s]
            return (concat $ take 1 $ lines full, tag_ "pre" full)

        gitPatchExtra s (Just p) = traced "gitPatchExtra Just" $ do
            mirror <- gitInitMirror
            Stdout full <- cmd (Cwd mirror) "git diff" (fromSHA1 s ++ ".." ++ fromSHA1 p)
            Stdout numstat <- cmd (Cwd mirror) "git diff --numstat" (fromSHA1 s ++ ".." ++ fromSHA1 p)
            let xs = [x | [_,_,x] <- map words $ lines numstat]
            return (unwordsLimit 3 xs, tag_ "pre" full)
