{-# LANGUAGE FlexibleInstances #-}

module Example(main, platforms) where

import Development.Bake
import Development.Shake.Command
import System.Environment.Extra
import System.FilePath
import Data.List.Extra
import Data.Tuple.Extra
import System.Directory
import Control.Monad.Extra
import Data.Maybe
import System.Time.Extra

useStep = True

data Platform = Linux | Windows deriving (Show,Read)
data Action = Compile | Run Int deriving (Show,Read)

instance Stringy (Platform, Action) where
    stringyTo (a,b) = show a ++ " " ++ show b
    stringyFrom = (read *** read) . word1


platforms = [Linux,Windows]

main :: IO ()
main = do
    let err = "You need to set an environment variable named $REPO for the Git repo"
    repo <- fromMaybe (error err) `fmap` lookupEnv "REPO"
    bake $
        ovenPretty $
        (if useStep
            then ovenStepGit compile repo "master" Nothing ["dist"]
            else ovenIncremental . ovenGit repo "master" Nothing) $
        ovenNotifyStdout $
        ovenTest (return allTests) execute
        defaultOven{ovenServer=("127.0.0.1",5000)}

allTests = [(p,t) | p <- platforms, t <- Compile : map Run [1,10,0]]

compile :: IO [FilePath]
compile = do
    createDirectoryIfMissing True "dist"
    unit $ cmd "ghc --make Main.hs -o dist/Main"
    -- ghc --make only has 1 second timestamp resolution
    -- so sleep for a second to make sure we work with incremental compilation
    sleep 1
    return ["dist"]

execute :: (Platform,Action) -> TestInfo (Platform,Action)
execute (p,Compile) = require [show p] $ run $ unless useStep $ do
    incrementalStart
    compile
    incrementalDone
execute (p,Run i) = depend [(p,Compile)] $ require [show p] $ run $
    cmd ("dist" </> "Main") (show i)
