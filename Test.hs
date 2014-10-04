
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
import Data.List
import Data.Char


---------------------------------------------------------------------
-- NORMAL CODE

data Platform = Linux | Windows deriving (Show,Read)
data Action = Compile | Run Int deriving (Show,Read)

platforms = [Linux]

main :: IO ()
main = do
    args <- getArgs
    dir <- fmap (intercalate "/" . takeWhile (/= ".bake-test") . splitDirectories) $ getCurrentDirectory
    if null args then test (dir ++ "/.bake-test") else bake $
        ovenGit (dir ++ "/.bake-test/repo") "master" $
        ovenTest readShowStringy (return allTests) execute
        defaultOven{ovenServer=("127.0.0.1",5000)}

allTests = [(p,t) | p <- platforms, t <- Compile : map Run [1..3]]

execute :: (Platform,Action) -> TestInfo (Platform,Action)
execute (p,Compile) = matchOS p $ run $ do
    cmd "ghc --make Main.hs"
execute (p,Run i) = require [(p,Compile)] $ matchOS p $ run $ do
    cmd ("." </> "Main") (show i)

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
        () <- cmd "git config user.email" ["master@example.com"]
        () <- cmd "git config user.name" ["The Master"]
        writeFile "Main.hs" "module Main where\n\n-- Entry point\nmain = print 1\n"
        () <- cmd "git add Main.hs"
        () <- cmd "git commit -m" ["Initial version"]
        () <- cmd "git checkout -b none" -- so I can git push to master
        return ()

    forM_ ["bob","tony"] $ \s -> do
        createDirectoryIfMissing True (dir </> "repo-" ++ s)
        withCurrentDirectory (dir </> "repo-" ++ s) $ do
            print "clone"
            () <- cmd "git clone ../repo ."
            () <- cmd "git config user.email" [s ++ "@example.com"]
            () <- cmd "git config user.name" ["Mr " ++ toUpper (head s) : map toLower (tail s)]
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
    createDirectoryIfMissing True $ dir </> "server"
    p0 <- createProcessAlive (proc exe ["server"]){cwd=Just $ dir </> "server"}
    environment <- getEnvironment
    ps <- forM platforms $ \p -> do
        sleep 0.5 -- so they don't ping at the same time
        createDirectoryIfMissing True $ dir </> "client-" ++ show p
        createProcessAlive (proc exe ["client","--ping=1","--name=" ++ show p])
            {cwd=Just $ dir </> "client-" ++ show p,env=Just $ ("PLATFORM",show p) : environment}
    flip finally (do writeIORef aborting True; mapM_ terminateProcess $ p0 : ps) $ do
        let edit name act = withCurrentDirectory (dir </> "repo-" ++ name) $ do
                act
                () <- cmd "git add *"
                () <- cmd "git commit -m" ["Update from " ++ name]
                () <- cmd "git push origin" name
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

        sleep 10
        withTempDir $ \d -> withCurrentDirectory d $ do
            unit $ cmd "git clone" (dir </> "repo") "."
            unit $ cmd "git checkout master"
            src <- readFile "Main.hs"
            let expect = "module Main(main) where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n"
            when (src /= expect) $ do
                error $ "Expected to have updated Main, but got:\n" ++ src

        putStrLn "Completed successfully!"
