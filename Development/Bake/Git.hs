
module Development.Bake.Git(SHA1, ovenGit) where

import Development.Bake.Type
import Development.Shake.Command


newtype SHA1 = SHA1 String deriving Show

sha1 :: String -> SHA1
sha1 x | length x /= 40 = error $ "SHA1 for Git must be 40 characters long, got " ++ show x
       | not $ all (`elem` "0123456789abcdef") x = error $ "SHA1 for Git must be all lower case hex, got " ++ show x 
       | otherwise = SHA1 x

stringySHA1 :: Stringy SHA1
stringySHA1 = Stringy
    {stringyTo = \(SHA1 x) -> x
    ,stringyFrom = sha1
    ,stringyPretty = \(SHA1 x) -> take 7 x
    ,stringyExtra = const $ return ""
    }


-- | Given a repo name, and a set of tests, produce something that runs from git
ovenGit :: String -> String -> Oven () () test -> Oven SHA1 SHA1 test
ovenGit repo branch o = o
    {ovenUpdateState = gitUpdateState
    ,ovenPrepare = \c -> do gitCheckout c; ovenPrepare o (down c)
    ,ovenStringyState = stringySHA1
    ,ovenStringyPatch = stringySHA1
    }
    where
        down (Candidate s ps) = Candidate () $ map (const ()) ps

        gitUpdateState Nothing = do
            Stdout hash <- cmd "git ls-remote" repo ("refs/heads/" ++ branch)
            case words hash of
                [] -> error "Couldn't find branch"
                x:xs -> return $ SHA1 x


        gitUpdateState _ = error "gitUpdateState with Just"

        gitCheckout = error "gitCheckout"
