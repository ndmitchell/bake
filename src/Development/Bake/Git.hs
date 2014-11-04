{-# LANGUAGE ViewPatterns #-}

module Development.Bake.Git(
    SHA1, ovenGit,
    ) where

import Development.Bake.Core.Type
import General.Extra
import Development.Shake.Command
import Control.Monad.Extra
import Data.List.Extra
import General.Format
import System.Directory.Extra
import System.FilePath
import Data.Maybe
import Data.Tuple.Extra
import Data.Char
import Data.Hashable


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
            Stdout hash <- cmd "git ls-remote" [repo] [branch]
            case words $ concat $ takeEnd 1 $ lines hash of
                [] -> error "Couldn't find branch"
                x:xs -> return $ sha1 $ trim x

        gitUpdateState (Just (s, ps)) = traced "gitUpdateState Just" $ do
            gitCheckout s ps
            Stdout x <- cmd (Cwd path) "git rev-parse" [branch]
            unit $ cmd (Cwd path) "git push" [repo] [branch ++ ":" ++ branch]
            return $ sha1 $ trim x

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
            when (trim x /= fromSHA1 s) $ error "Branch changed while running"
            forM_ ps $ \p ->
                unit $ cmd (Cwd path) "git merge" (fromSHA1 p)

        gitPatchExtra s Nothing = traced "gitPatchExtra Nothing" $ do
            mirror <- gitInitMirror
            Stdout full <- cmd (Cwd mirror) "git log -n3" [fromSHA1 s]
            return (concat $ take 1 $ lines full, tag_ "pre" full)

        gitPatchExtra (SHA1 s) (Just (SHA1 p)) = traced "gitPatchExtra Just" $ do
            mirror <- gitInitMirror
            Stdout diff <- cmd (Cwd mirror)
                "git diff" [s ++ ".." ++ p]
            Stdout stat <- cmd (Cwd mirror)
                "git diff --stat" [s ++ ".." ++ p]
            Stdout log <- cmd (Cwd mirror)
                "git log --no-merges -n1 --pretty=format:%s" [p]
            return (reduceStat stat ++ "<br />\n" ++ takeWhile (/= '\n') log
                   ,tag_ "pre" $ prettyStat stat ++ "\n" ++ prettyDiff diff)


---------------------------------------------------------------------
-- DIFF UTILITIES

reduceStat :: String -> String
reduceStat = commasLimit 3 . map trim . map (takeWhile (/= '|')) . dropEnd 1 . lines


diff :: FilePath -> String
diff x = "diff:" ++ show (abs $ hash x)

-- |
-- > src/Paths.hs                          |   11 ++
-- > src/Test.hs                           |  258 ++++++++++++------------
-- > travis.hs                             |    4 +-
-- > 28 files changed, 1612 insertions(+), 1302 deletions(-)
prettyStat :: String -> String
prettyStat = unlines . maybe [] (uncurry snoc . first (map f)) . unsnoc . map trimStart . lines
    where
        f x = tag "a" ["href=#" ++ diff a] a ++ b ++ g c
            where (ab,c) = break (== '|') x
                  (a,b) = spanEnd isSpace ab
        g x@('+':_) = tag "span" ["class=green"] a ++ g b
            where (a,b) = span (== '+') x
        g x@('-':_) = tag "span" ["class=red"] a ++ g b
            where (a,b) = span (== '-') x
        g (x:xs) = x : g xs
        g [] = []


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
prettyDiff :: String -> String
prettyDiff = unlines . map f . lines
    where
        f x | "diff --git " `isPrefixOf` x =
            let files = [y | ab:'/':y <- drop 2 $ words x, ab `elem` "ab"] in
            tag "a" (take 1 ["name=" ++ diff y | y <- files]) "" ++
            tag_ "b" x
        f x | any (`isPrefixOf` x) ["index ","--- ","+++ "] = tag_ "b" x
        f xs@('+':_) = tag "span" ["class=green"] xs
        f xs@('-':_) = tag "span" ["class=red"] xs
        f xs = xs
