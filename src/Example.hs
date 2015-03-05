
module Example(main, platforms) where

import Development.Bake
import Development.Shake.Command
import System.Environment.Extra
import System.FilePath
import Data.List.Extra
import System.Directory
import Control.Arrow
import Control.Monad.Extra
import Data.Maybe
import System.Time.Extra

-- if True then use ovenStep, otherwise use ovenIncremental
useStep = True


data Platform = Linux | Windows deriving (Show,Read)
data Action = Compile | Run Int deriving (Show,Read)

platforms = [Linux,Windows]

main :: IO ()
main = do
    let err = "You need to set an environment variable named $REPO for the Git repo"
    repo <- fromMaybe (error err) `fmap` lookupEnv "REPO"
    bake $
        (if useStep then id else ovenIncremental) $
        ovenPretty "=" $
        ((if useStep then ovenStepGit compile else ovenGit) repo "master" Nothing) $
        ovenNotifyStdout $
        ovenTest testStringy (return allTests) execute
        defaultOven{ovenServer=("127.0.0.1",5000)}

testStringy = Stringy shw rd shw
    where shw (a,b) = show a ++ " " ++ show b
          rd x = (read *** read) $ word1 x

allTests = [(p,t) | p <- platforms, t <- Compile : map Run [1,10,0]]

compile :: IO [FilePath]
compile = do
    createDirectoryIfMissing True "dist"
    unit $ cmd "ghc --make Main.hs -o dist/Main"
    sleep 1
    return ["dist"]

execute :: (Platform,Action) -> TestInfo (Platform,Action)
execute (p,Compile) = matchOS p $ run $ unless useStep $ do
    () <- cmd "ghc --make Main.hs"
    -- ghc --make only has 1 second timestamp resolution
    -- so sleep for a second to make sure we work with incremental
    sleep 1
    incrementalDone
execute (p,Run i) = require [(p,Compile)] $ matchOS p $ run $
    if useStep then do
        [dist] <- stepGet
        cmd (dist </> "dist" </> "Main") (show i)
    else
        cmd ("." </> "Main") (show i)

-- So we can run both clients on one platform we use an environment variable
-- to fake changing OS
matchOS :: Platform -> TestInfo t -> TestInfo t
matchOS p = suitable (fmap (== show p) $ getEnv "PLATFORM")
