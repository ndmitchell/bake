
module Development.Bake.Git(SHA1, ovenGit) where

import Development.Bake.Type
import Development.Shake.Command
import Control.Monad.Extra
import Data.List.Extra
import Development.Bake.Format
import System.Directory
import Data.Hashable
import System.FilePath
import Data.Maybe


newtype SHA1 = SHA1 {fromSHA1 :: String} deriving (Show,Eq)

sha1 :: String -> SHA1
sha1 x | length x /= 40 = error $ "SHA1 for Git must be 40 characters long, got " ++ show x
       | not $ all (`elem` "0123456789abcdef") x = error $ "SHA1 for Git must be all lower case hex, got " ++ show x 
       | otherwise = SHA1 x

stringySHA1 :: Stringy SHA1
stringySHA1 = Stringy
    {stringyTo = \(SHA1 x) -> x
    ,stringyFrom = sha1
    ,stringyPretty = \(SHA1 x) -> take 7 x
    }


-- | Modify an 'Oven' to work with the Git version control system.
--   Requires the name of the repo (e.g. @https:\/\/github.com\/ndmitchell\/bake.git@)
--   and the name of a branch (e.g. @master@). You can optionally give a path fragment
--   which is used to clone into.
ovenGit :: String -> String -> Maybe FilePath -> Oven () () test -> Oven SHA1 SHA1 test
ovenGit repo branch path o = o
    {ovenUpdateState = gitUpdateState
    ,ovenPrepare = \s ps -> do gitCheckout s ps; ovenPrepare o () $ map (const ()) ps
    ,ovenPatchExtra = gitPatchExtra
    ,ovenStringyState = stringySHA1
    ,ovenStringyPatch = stringySHA1
    }
    where
        -- the directory where my git repo mirror is stored
        mirror = "../bake-git-" ++ show (hash (repo, branch))
        cwd = fmap Cwd path

        -- initialise the mirror, or make it up to date
        gitInitMirror = do
            print "gitInitMirror"
            createDirectoryIfMissing True mirror
            writeFile (mirror <.> "txt") (show (repo, branch))
            b <- doesDirectoryExist $ mirror </> ".git"
            if b then
                unit $ cmd (Cwd mirror) "git fetch"
             else do
                print ("git clone","repo",repo)
                unit $ cmd (Cwd mirror) "git clone" repo "."
                unit $ cmd (Cwd mirror) "git config user.email" ["https://github.com/ndmitchell/bake"]
                unit $ cmd (Cwd mirror) "git config user.name" ["Bake Continuous Integration"]
            print "gitInitMirror end"

        -- initialise a copy of the mirror, or make it up to date
        gitInitCopy = do
            gitInitMirror
            print "gitInitCopy"
            b <- doesDirectoryExist ".git"
            if b then
                unit $ cmd cwd "git fetch"
             else do
                unit $ cmd "git clone" repo $ fromMaybe "." path
                unit $ cmd cwd "git config user.email" ["https://github.com/ndmitchell/bake"]
                unit $ cmd cwd "git config user.name" ["Bake Continuous Integration"]

        gitUpdateState Nothing = do
            print "gitUpdateState Nothing"
            gitInitMirror
            Stdout hash <- cmd (Cwd mirror) "git rev-parse" ("refs/heads/" ++ branch)
            case words hash of
                [] -> error "Couldn't find branch"
                x:xs -> return $ sha1 $ strip x

        gitUpdateState (Just (s, ps)) = do
            print "gitUpdateState Just"
            gitCheckout s ps
            Stdout x <- cmd cwd "git rev-parse HEAD"
            unit $ cmd cwd "git checkout -b bake-temp"
            unit $ cmd cwd "git checkout -B master bake-temp"
            unit $ cmd cwd "git push origin master --force"
            unit $ cmd cwd "git branch -D bake-temp"
            return $ sha1 $ strip x

        gitCheckout s ps = do
            print "gitCheckout"
            gitInitCopy
            unit $ cmd cwd "git checkout" (fromSHA1 s)
            forM_ ps $ \p ->
                unit $ cmd cwd "git merge" (fromSHA1 p)

        gitPatchExtra p = do
            gitInitMirror
            print "gitPatchExtra"
            Stdout full <- cmd (Cwd mirror) "git diff" (branch ++ ".." ++ fromSHA1 p)
            Stdout numstat <- cmd (Cwd mirror) "git diff --numstat" (branch ++ ".." ++ fromSHA1 p)
            let xs = [x | [_,_,x] <- map words $ lines numstat]
            return (unwordsLimit 3 xs, tag_ "pre" full)
