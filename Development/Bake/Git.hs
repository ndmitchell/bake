
module Development.Bake.Git(SHA1, ovenGit) where

import Development.Bake.Type
import Data.Monoid
import Development.Shake.Command


newtype SHA1 = SHA1 String deriving (Read,Show)

stringySHA1 = readShowStringy


-- | Given a repo name, and a set of tests, produce something that runs from git
ovenGit :: String -> String -> Oven () () test -> Oven SHA1 SHA1 test
ovenGit repo branch o = o
    {ovenUpdateState = gitUpdateState
    ,ovenRunTest = \c t -> case t of
        Nothing -> gitCheckout c `mappend` ovenRunTest o (down c) Nothing
        _ -> ovenRunTest o (down c) t
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
