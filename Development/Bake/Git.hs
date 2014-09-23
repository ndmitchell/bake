
module Development.Bake.Git(SHA1, ovenGit) where

import Development.Bake.Type

newtype SHA1 = SHA1 String deriving (Read,Show)


-- | Given a repo name, and a set of tests, produce something that runs from git
ovenGit :: String -> String -> Oven SHA1 SHA1 test -> Oven SHA1 SHA1 test
ovenGit repo branch = undefined
