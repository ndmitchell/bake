
module Example(main, platforms) where

import Development.Bake
import Development.Shake.Command
import System.Environment
import System.FilePath
import Data.List.Extra
import System.Directory
import Control.Arrow
import Control.Monad
import Data.Maybe


data Platform = Linux | Windows deriving (Show,Read)
data Action = Compile | Run Int deriving (Show,Read)

platforms = [Linux,Windows]

main :: IO ()
main = do
    let err = "You need to set an environment variable named $REPO for the Git repo"
    repo <- fromMaybe (error err) `fmap` lookupEnv "REPO"
    bake $
        ovenIncremental $
        ovenPretty "=" $
        ovenGit repo "master" Nothing $
        ovenNotifyStdout $
        ovenTest testStringy (return allTests) execute
        defaultOven{ovenServer=("127.0.0.1",5000)}

testStringy = Stringy shw rd shw
    where shw (a,b) = show a ++ " " ++ show b
          rd x = (read *** read) $ word1 x

allTests = [(p,t) | p <- platforms, t <- Compile : map Run [1,10,0]]

execute :: (Platform,Action) -> TestInfo (Platform,Action)
execute (p,Compile) = matchOS p $ run $ do
    -- ghc --make isn't a good citizen of incremental
    -- so we remove all the generated files to force the rebuild
    Exit _ <- cmd "rm *Main.o *Main.hi *Main.exe *Main"
    copyFile "Main.hs" "Main.bup"
    (Stdout s1, Stderr s2) <- cmd "ghc --make Main.hs"
    writeFile "Main.comp" (s1 ++ s2)
    incrementalDone
execute (p,Run i) = require [(p,Compile)] $ matchOS p $ run $ do
    when (i == 10) $ do
        x <- getCurrentDirectory
        print x
        print =<< readFile (x <.> "txt")
        print =<< readFile "Main.hs"
        print =<< readFile "Main.comp"
        print =<< readFile "Main.bup"
    cmd ("." </> "Main") (show i)

-- So we can run both clients on one platform we use an environment variable
-- to fake changing OS
matchOS :: Platform -> TestInfo t -> TestInfo t
matchOS p = suitable (fmap (== show p) $ getEnv "PLATFORM")
