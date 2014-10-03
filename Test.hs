
module Main(main) where

import Development.Bake
import Development.Bake.Git
import Development.Shake.Command
import Development.Bake.Util
import System.Directory
import System.Environment
import System.FilePath
import Control.Monad
import Control.Exception
import Control.Concurrent
import System.Process
import Data.IORef


---------------------------------------------------------------------
-- NORMAL CODE

data Platform = Linux | Windows deriving (Show,Read)
data Action = Compile | Run Int deriving (Show,Read)

platforms = [Linux]

main :: IO ()
main = do
    args <- getArgs
    let dir = "C:/Neil/bake/.bake-test"
    if null args then test dir else bake $
        ovenGit (dir ++ "/repo") "master" $
        ovenTest readShowStringy (return allTests) execute
        defaultOven

allTests = [(p,t) | p <- platforms, t <- Compile : map Run [1..3]]

execute :: (Platform,Action) -> TestInfo (Platform,Action)
execute (p,Compile) = matchOS p $ run $ do
    cmd "ghc --make Main.hs"
execute (p,Run i) = require [(p,Compile)] $ matchOS p $ run $ do
    cmd "Main" (show i)

matchOS :: Platform -> TestInfo t -> TestInfo t
matchOS p = suitable (fmap (== show p) $ getEnv "PLATFORM")


---------------------------------------------------------------------
-- TEST BITS

test :: FilePath -> IO ()
test dir = do
    b <- doesDirectoryExist dir
    when b $ do
        () <- cmd "chmod -R 755 .bake-test"
        () <- cmd "rm -rf .bake-test"
        return ()

    createDirectoryIfMissing True (dir </> "repo")
    withCurrentDirectory (dir </> "repo") $ do
        () <- cmd "git init"
        writeFile "Main.hs" "module Main where\n\n-- Entry point\nmain = print 1\n"
        () <- cmd "git add Main.hs"
        () <- cmd "git commit -m" ["Initial version"]
        return ()

    forM_ ["bob","tony"] $ \s -> do
        createDirectoryIfMissing True (dir </> s)
        withCurrentDirectory (dir </> s) $ do
            print "clone"
            () <- cmd "git clone ../repo ."
            print "checkout"
            () <- cmd "git checkout -b" s
            return ()

    aborting <- newIORef False
    let createProcessAlive p = do
            t <- myThreadId
            (_,_,_,pid) <- createProcess p
            forkIO $ do
                waitForProcess pid
                b <- readIORef aborting
                when (not b) $ throwTo t $ ErrorCall "Process died"
            sleep 2
            return pid
    exe <- getExecutablePath
    p0 <- createProcessAlive (proc exe ["server"]){cwd=Just dir}
    environment <- getEnvironment
    ps <- forM platforms $ \p -> do
        sleep 0.5 -- so they don't ping at the same time
        createProcessAlive (proc exe ["client","--ping=1","--name=" ++ show p])
            {cwd=Just dir,env=Just $ ("PLATFORM",show p) : environment}
    flip finally (do writeIORef aborting True; mapM_ terminateProcess $ p0 : ps) $ do

    let edit name act = withCurrentDirectory (dir </> name) $ do
            act
            () <- cmd "git add *"
            () <- cmd "git commit -m" ["Update from " ++ name]
            Stdout sha1 <- cmd "git rev-parse HEAD"
            print "adding patch"
            () <- cmd exe "addpatch" ("--name=" ++ sha1) ("--author=" ++ name)
            return ()

    putStrLn "% MAKING EDIT AS BOB"
    edit "bob" $
        writeFile "Main.hs" "module Main(main) where\n\n-- Entry point\nmain = print 1\n"
    sleep 2
    putStrLn "% MAKING EDIT AS TONY"
    edit "tony" $
        writeFile "Main.hs" "module Main where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n"

    sleep 30

    putStrLn "Script finished, press any key to exit..."
    getLine
    return ()
