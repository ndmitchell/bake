
module Development.Bake.Git(SHA1, ovenGit) where

import Development.Bake.Type
import Development.Shake.Command
import Control.Monad.Extra
import Data.List.Extra
import Development.Bake.Format


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
--   and the name of a branch (e.g. @master@).
ovenGit :: String -> String -> Oven () () test -> Oven SHA1 SHA1 test
ovenGit repo branch o = o
    {ovenUpdateState = gitUpdateState
    ,ovenPrepare = \s ps -> do gitCheckout s ps; ovenPrepare o () $ map (const ()) ps
    ,ovenPatchExtra = gitPatchExtra
    ,ovenStringyState = stringySHA1
    ,ovenStringyPatch = stringySHA1
    }
    where
        gitUpdateState Nothing = do
            Stdout hash <- cmd "git ls-remote" repo ("refs/heads/" ++ branch)
            case words hash of
                [] -> error "Couldn't find branch"
                x:xs -> return $ sha1 $ strip x

        gitUpdateState (Just (s, ps)) = do
            gitCheckout s ps
            Stdout x <- cmd "git rev-parse HEAD"
            unit $ cmd "git checkout -b temp"
            unit $ cmd "git checkout -B master temp"
            unit $ cmd "git push origin master --force"
            return $ sha1 $ strip x

        gitCheckout s ps = do
            unit $ cmd "git clone" repo "."
            unit $ cmd "git config user.email" ["https://github.com/ndmitchell/bake"]
            unit $ cmd "git config user.name" ["Bake Continuous Integration"]
            unit $ cmd "git checkout" (fromSHA1 s)
            forM_ ps $ \p ->
                unit $ cmd "git merge" (fromSHA1 p)

        gitPatchExtra p = do
            unit $ cmd "git clone" repo "."
            Stdout full <- cmd "git diff" ("origin/" ++ branch ++ ".." ++ fromSHA1 p)
            Stdout numstat <- cmd "git diff --numstat" ("origin/" ++ branch ++ ".." ++ fromSHA1 p)
            let xs = [x | [_,_,x] <- map words $ lines numstat]
            return (unwordsLimit 3 xs, tag_ "pre" full)
