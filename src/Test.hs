{-# LANGUAGE CPP #-}

module Test(main) where

import Development.Shake.Command
import System.Directory.Extra
import System.IO.Extra
import System.Time.Extra
import System.Environment.Extra
import System.FilePath
import Control.Monad.Extra
import Control.Exception.Extra
import Control.Concurrent
import System.Process
import Data.List.Extra
import Data.IORef
import Data.Char
import Control.Applicative
import qualified Example
import Prelude

import Development.Bake.Test.Simulate


main :: IO ()
main = do
    args <- getArgs
    if args /= [] then Example.main else do
        simulate
        dir <- getCurrentDirectory
        test $ dir ++ "/.bake-test"

test :: FilePath -> IO ()
test dir = do
    let repo = "file:///" ++ dropWhile (== '/') (replace "\\" "/" dir) ++ "/repo"
    b <- doesDirectoryExist dir
    when b $ do
        unit $ cmd "chmod -R 755 .bake-test"
        unit $ cmd "rm -rf .bake-test"
        return ()
#if __GLASGOW_HASKELL__ >= 708
    setEnv "http_proxy" ""
#endif

    createDirectoryIfMissing True (dir </> "repo")
    withCurrentDirectory (dir </> "repo") $ do
        unit $ cmd "git init"
        unit $ cmd "git config user.email" ["gwen@example.com"]
        unit $ cmd "git config user.name" ["Ms Gwen"]
        writeFile "Main.hs" "module Main where\n\n-- Entry point\nmain = print 1\n"
        unit $ cmd "git add Main.hs"
        unit $ cmd "git commit -m" ["Initial version"]
        unit $ cmd "git checkout -b none" -- so I can git push to master

    forM_ ["bob","tony"] $ \s -> do
        createDirectoryIfMissing True (dir </> "repo-" ++ s)
        withCurrentDirectory (dir </> "repo-" ++ s) $ do
            print "clone"
            unit $ cmd "git clone" repo "."
            unit $ cmd "git config user.email" [s ++ "@example.com"]
            unit $ cmd "git config user.name" ["Mr " ++ toUpper (head s) : map toLower (tail s)]
            unit $ cmd "git checkout -b" s

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
    curdir <- getCurrentDirectory
    environment <- (\env -> ("REPO",repo):("bake_datadir",curdir):env) <$> getEnvironment
    p0 <- createProcessAlive (proc exe ["server","--author=admin"])
        {cwd=Just $ dir </> "server", env=Just environment}
    sleep 5
    ps <- forM (zip [1..] Example.platforms) $ \(i,p) -> do
        sleep 0.5 -- so they don't ping at the same time
        createDirectoryIfMissing True $ dir </> "client-" ++ show p
        createProcessAlive (proc exe $
            ["client","--ping=1","--name=" ++ show p,"--threads=" ++ show i,"--provide=" ++ show p])
            {cwd=Just $ dir </> "client-" ++ show p,env=Just environment}
    flip finally (do writeIORef aborting True; mapM_ terminateProcess $ p0 : ps) $ do
        let edit name act = withCurrentDirectory (dir </> "repo-" ++ name) $ do
                act
                unit $ cmd "git add *"
                unit $ cmd "git commit -m" ["Update from " ++ name]
                unit $ cmd "git push origin" name
                Stdout sha1 <- cmd "git rev-parse HEAD"
                unit $ cmd exe "addpatch" ("--name=" ++ name ++ "=" ++ sha1) ("--author=" ++ name)

        putStrLn "% MAKING EDIT AS BOB"
        edit "bob" $
            writeFile "Main.hs" "module Main(main) where\n\n-- Entry point\nmain = print 1\n"
        sleep 2
        putStrLn "% MAKING EDIT AS TONY"
        edit "tony" $
            writeFile "Main.hs" "module Main where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n"

        retry 10 $ do
            sleep 10
            withTempDir $ \d -> withCurrentDirectory d $ do
                unit $ cmd "git clone" repo "."
                unit $ cmd "git checkout master"
                src <- readFile "Main.hs"
                let expect = "module Main(main) where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n"
                when (src /= expect) $ do
                    error $ "Expected to have updated Main, but got:\n" ++ src

        unit $ cmd exe "pause"
        putStrLn "% MAKING A GOOD EDIT AS BOB"
        edit "bob" $ do
            unit $ cmd "git fetch origin"
            unit $ cmd "git merge origin/master"
            writeFile "Main.hs" "module Main(main) where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n\n"
        putStrLn "% MAKING A BAD EDIT AS BOB"
        edit "bob" $
            writeFile "Main.hs" "module Main(main) where\nimport System.Environment\n-- Entry point\nmain :: IO ()\nmain = do [[_]] <- getArgs; print 1\n\n"
        putStrLn "% MAKING A GOOD EDIT AS TONY"
        edit "tony" $ do
            unit $ cmd "git fetch origin"
            unit $ cmd "git merge origin/master"
            writeFile "Main.hs" "-- Tony waz ere\nmodule Main(main) where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n"
        putStrLn "% MAKING A MERGE CONFLICT AS BOB"
        edit "bob" $
            writeFile "Main.hs" "-- Bob waz ere\nmodule Main(main) where\nimport System.Environment\n-- Entry point\nmain :: IO ()\nmain = do [[_]] <- getArgs; print 1\n\n"
        putStrLn "% MAKING ANOTHER GOOD EDIT AS TONY"
        edit "tony" $ do
            writeFile "Main.hs" "-- Tony waz ere 1981\nmodule Main(main) where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n"
        unit $ cmd exe "unpause"

        retry 15 $ do
            sleep 10
            withTempDir $ \d -> withCurrentDirectory d $ do
                unit $ cmd "git clone" repo "."
                unit $ cmd "git checkout master"
                src <- readFile "Main.hs"
                let expect = "-- Tony waz ere 1981\nmodule Main(main) where\n\n-- Entry point\nmain :: IO ()\nmain = print 1\n\n"
                when (src /= expect) $ do
                    error $ "Expected to have updated Main, but got:\n" ++ src

        putStrLn "Completed successfully!"
        -- putStrLn "Waiting (time to view webpage)..." >> sleep 120
