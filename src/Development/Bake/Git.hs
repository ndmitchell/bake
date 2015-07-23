{-# LANGUAGE ViewPatterns #-}

module Development.Bake.Git(
    SHA1(..), sha1, ovenGit,
    gitPatchExtra, gitInit
    ) where

import Development.Bake.Core.Type
import General.Extra
import Development.Shake.Command
import Control.Monad.Extra
import Data.List.Extra
import General.HTML
import System.Directory.Extra
import System.FilePath
import Data.Maybe
import Data.Tuple.Extra
import Data.Char
import Data.Hashable
import Data.Monoid
import Prelude


newtype SHA1 = SHA1 {fromSHA1 :: String} deriving (Show,Eq)

sha1 :: String -> SHA1
sha1 ('\'': x) = sha1' x
sha1 x = sha1' x

sha1' :: String -> SHA1
sha1' x | length x /= 40 = error $ "SHA1 for Git must be 40 characters long, got " ++ show x
        | not $ all (`elem` "0123456789abcdef") x = error $ "SHA1 for Git must be all lower case hex, got " ++ show x 
        | otherwise = SHA1 x

instance Stringy SHA1 where
    stringyTo = fromSHA1
    stringyPretty = take 7 . fromSHA1
    stringyFrom = sha1


-- | Modify an 'Oven' to work with the Git version control system.
--   Requires the name of the repo (e.g. @https:\/\/github.com\/ndmitchell\/bake.git@)
--   and the name of a branch (e.g. @master@). You can optionally give a path fragment
--   which is used to clone into.
ovenGit :: String -> String -> Maybe FilePath -> Oven () () test -> Oven SHA1 SHA1 test
ovenGit repo branch (fromMaybe "." -> path) o = o
    {ovenInit = gitInit repo branch
    ,ovenUpdate = gitUpdate
    ,ovenPrepare = \s ps -> do gitCheckout s ps; ovenPrepare o () $ map (const ()) ps
    ,ovenPatchExtra = \s p -> gitPatchExtra s p =<< gitInitMirror
    ,ovenSupersede = \_ _ -> False
    }
    where
        gitSafe dir = do
            time_ $ cmd (Cwd dir) "git config user.email" ["https://github.com/ndmitchell/bake"]
            time_ $ cmd (Cwd dir) "git config user.name" ["Bake Continuous Integration"]

        -- initialise the mirror, or make it up to date
        gitInitMirror = traced "gitInitMirror" $ do
            -- make sure we descend one directory, since bake writes .bake.name
            mirror <- fmap (</> "mirror") $ createDir "../bake-git" [repo]
            createDirectoryIfMissing True mirror
            -- see http://blog.plataformatec.com.br/2013/05/how-to-properly-mirror-a-git-repository/
            ready <- doesFileExist $ mirror </> "HEAD"
            if ready then
                time_ $ cmd (Cwd mirror) "git fetch --prune"
             else do
                time_ $ cmd (Cwd mirror) "git clone --mirror" [repo] "."
                gitSafe mirror
            return mirror

        gitUpdate s ps = traced "gitUpdate" $ do
            gitCheckout s ps
            Stdout x <- time $ cmd (Cwd path) "git rev-parse" [branch]
            time_ $ cmd (Cwd path) "git push" [repo] [branch ++ ":" ++ branch]
            return $ sha1 $ trim x

        gitCheckout s ps = traced "gitCheckout" $ do
            createDirectoryIfMissing True path
            mirror <- gitInitMirror
            unlessM (doesDirectoryExist $ path </>".git") $ do
                time_ $ cmd (Cwd path) "git init"
                gitSafe path
                time_ $ cmd (Cwd path) "git remote add origin" [(if path == "." then "" else "../") ++ mirror]
            time_ $ cmd (Cwd path) "git fetch"
            time_ $ cmd (Cwd path) "git reset" -- to unwedge a previous merge conflict
            time_ $ cmd (Cwd path) "git checkout" [branch]
            time_ $ cmd (Cwd path) "git reset --hard" ["origin/" ++ branch]
            Stdout x <- time $ cmd (Cwd path) "git rev-parse HEAD"
            when (trim x /= fromSHA1 s) $ error $
                "The branch " ++ branch ++ " changed SHA1 independently of bake.\n" ++
                "Expected value: " ++ fromSHA1 s ++ "\n" ++
                "But has become: " ++ trim x
            forM_ ps $ \p ->
                time_ $ cmd (Cwd path) "git merge" (fromSHA1 p)


gitInit :: String -> String -> IO SHA1
gitInit repo branch = traced "gitInit" $ do
    Stdout hash <- time $ cmd "git ls-remote" [repo] [branch]
    case words $ concat $ takeEnd 1 $ lines hash of
        [] -> error "Couldn't find branch"
        x:xs -> return $ sha1 $ trim x


traced :: String -> IO a -> IO a
traced msg act = do
    putStrLn $ "% GIT: Begin " ++ msg
    res <- act
    putStrLn $ "% GIT: Finish " ++ msg
    return res


---------------------------------------------------------------------
-- DIFF UTILITIES

gitPatchExtra :: SHA1 -> Maybe SHA1 -> FilePath -> IO (String, String)
gitPatchExtra s Nothing dir = do
    Stdout full <- time $ cmd (Cwd dir) "git log --no-merges -n10 --pretty=format:%s" [fromSHA1 s]
    Stdout count <- time $ cmd (Cwd dir) "git rev-list --count" [fromSHA1 s]
    let summary = takeWhile (/= '\n') full
    return (renderHTML $ do str_ $ count ++ " patches"; br_; str_ summary
           ,renderHTML $ pre_ $ str_ full)

gitPatchExtra s (Just p) dir = do
    Stdout diff <- time $ cmd (Cwd dir)
        "git diff" [fromSHA1 s ++ "..." ++ fromSHA1 p]
    Stdout stat <- time $ cmd (Cwd dir)
        "git diff --stat" [fromSHA1 s ++ "..." ++ fromSHA1 p]
    Stdout log <- time $ cmd (Cwd dir)
        "git log --no-merges -n1 --pretty=format:%s" [fromSHA1 p]
    return (renderHTML $ do str_ $ reduceStat stat; br_; str_ $ take 120 $ takeWhile (/= '\n') log
           ,renderHTML $ pre_ $ do prettyStat stat; str_ "\n"; prettyDiff diff)


reduceStat :: String -> String
reduceStat = commasLimit 3 . map trim . map (takeWhile (/= '|')) . dropEnd 1 . lines


diff :: FilePath -> String
diff x = "diff:" ++ show (abs $ hash x)

-- |
-- > src/Paths.hs                          |   11 ++
-- > src/Test.hs                           |  258 ++++++++++++------------
-- > travis.hs                             |    4 +-
-- > 28 files changed, 1612 insertions(+), 1302 deletions(-)
prettyStat :: String -> HTML
prettyStat = unlines_ . maybe [] (uncurry snoc . (map f *** str_)) . unsnoc . map trimStart . lines
    where
        f x = a__ [href_ $ "#" ++ diff a] (str_ a) <> str_ b <> g c
            where (ab,c) = break (== '|') x
                  (a,b) = spanEnd isSpace ab
        g x@('+':_) = span__ [class_ "green"] (str_ a) <> g b
            where (a,b) = span (== '+') x
        g x@('-':_) = span__ [class_ "red"] (str_ a) <> g b
            where (a,b) = span (== '-') x
        g (x:xs) = str_ [x] <> g xs
        g [] = mempty


-- |
-- > diff --git a/bake.cabal b/bake.cabal
-- > index 1aa1251..785cecc 100755
-- > --- a/bake.cabal
-- > +++ b/bake.cabal
-- > @@ -1,7 +1,7 @@
-- >  cabal-version:      >= 1.10
-- >  build-type:         Simple
-- >  name:               bake
-- > -version:            0.1
-- > +version:            0.2
prettyDiff :: String -> HTML
prettyDiff = unlines_ . map f . lines
    where
        f x | "diff --git " `isPrefixOf` x =
            let files = [y | ab:'/':y <- drop 2 $ words x, ab `elem` "ab"] in
            a__ (take 1 [name_ $ diff y | y <- files]) mempty <>
            b_ (str_ x)
        f x | any (`isPrefixOf` x) ["index ","--- ","+++ "] = b_ $ str_ x
        f xs@('+':_) = span__ [class_ "green"] $ str_ xs
        f xs@('-':_) = span__ [class_ "red"] $ str_ xs
        f xs = str_ xs
