{-# LANGUAGE RecordWildCards, DeriveDataTypeable, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Define a continuous integration system.
module Development.Bake.Core.Args(
    bake
    ) where

import System.Console.CmdArgs
import Development.Bake.Core.Type hiding (Client)
import Development.Bake.Core.Client
import Development.Bake.Core.GC
import Development.Bake.Server.Start
import Development.Bake.Core.Send
import Control.Exception.Extra
import General.Extra
import Control.DeepSeq
import System.FilePath
import Control.Monad.Extra
import Control.Applicative
import Data.Either.Extra
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Paths_bake
import Prelude


data Bake
    = Server {port :: Port, author :: [Author], timeout :: Double, admin :: String}
    | Client {host :: Host, port :: Port, author :: [Author], name :: String, threads :: Int, provide :: [String], ping :: Double}
    | AddPatch {host :: Host, port :: Port, author :: [Author], name :: String}
    | DelPatch {host :: Host, port :: Port, author :: [Author], name :: String}
    | DelPatches {host :: Host, port :: Port, author :: [Author]}
    | Requeue {host :: Host, port :: Port, author :: [Author]}
    | SetState {host :: Host, port :: Port, author :: [Author], state :: String}
    | Pause {host :: Host, port :: Port, author :: [Author]}
    | Unpause {host :: Host, port :: Port, author :: [Author]}
    | GC {bytes :: Integer, ratio :: Double, days :: Double, dirs :: [FilePath]}
    | Admin {password :: [String]}
      -- actions sent through from Bake itself
    | RunInit
    | RunUpdate {state :: String, patch :: [String]}
    | RunTest {test :: Maybe String, state :: String, patch :: [String]}
    | RunExtra {state :: String, patch :: [String]}
      deriving (Typeable,Data)


bakeMode = cmdArgsMode $ modes
    [Server{port = 0, author = [], timeout = 10*60, admin = ""}
    ,Client{host = "", threads = 1, name = "", ping = 60, provide = []}
    ,AddPatch{}
    ,DelPatch{}
    ,DelPatches{}
    ,Requeue{}
    ,SetState{state = ""}
    ,Pause{}
    ,Unpause{}
    ,GC 0 0 7 ([] &= args)
    ,Admin ([] &= args)
    ,RunTest def def def
    ,RunInit{}
    ,RunExtra{}
    ,RunUpdate{}
    ] &= verbosity

-- | The entry point to the system. Usually you will define:
--
-- > main = bake myOven
--
--   Where @myOven@ defines details about the server. The program
--   deals with command line arguments, run @--help@ for details.
bake :: (Stringy state, Stringy patch, Stringy test) => Oven state patch test -> IO ()
bake = bake_ -- so the forall's don't show up in Haddock

bake_ :: forall state patch test . (Stringy state, Stringy patch, Stringy test) => Oven state patch test -> IO ()
bake_ oven = do
    registerMaster
    timeInit
    getDataDir -- ensure it gets forced in case you change directory
    x <- cmdArgsRun bakeMode
    let author1 = head $ author x ++ ["unknown"]
    case x of
        Server{..} -> startServer (getPort port) author timeout admin oven
        Client{..} -> do
            name <- if name /= "" then return name else pick defaultNames
            startClient (getHostPort host port) author1 name threads provide ping oven
        AddPatch{..} -> sendAddPatch (getHostPort host port) author1 =<< check "patch" (undefined :: patch) name
        DelPatch{..} -> sendDelPatch (getHostPort host port) author1 =<< check "patch" (undefined :: patch) name
        DelPatches{..} -> sendDelAllPatches (getHostPort host port) author1
        Requeue{..} -> sendRequeue (getHostPort host port) author1
        SetState{..} -> sendSetState (getHostPort host port) author1 state
        Pause{..} -> sendPause (getHostPort host port) author1
        Unpause{..} -> sendUnpause (getHostPort host port) author1
        GC{..} -> garbageCollect bytes ratio (days * 24*60*60) (if null dirs then ["."] else dirs)
        Admin{..} -> do
            when (null password) $ putStrLn "Pass passwords on the command line to be suitable for 'server --admin=XXX'"
            forM_ password $ \x -> putStrLn $ "Password " ++ x ++ " requires --admin=" ++ encryptish x
        RunInit -> do
            s <- ovenInit oven
            writeFile ".bake.result" $ stringyTo s
        RunUpdate{..} -> do
            s <- ovenUpdate oven (stringyFrom state) $ map stringyFrom patch
            writeFile ".bake.result" $ stringyTo s
        RunTest{..} -> do
            case test of
                Nothing -> do
                    res <- nubOn stringyTo <$> ovenPrepare oven
                        (stringyFrom state)
                        (map stringyFrom patch)

                    -- check the patches all make sense
                    let follow t = map stringyTo $ testDepend $ ovenTestInfo oven $ stringyFrom t
                    whenJust (findCycle follow $ map stringyTo res) $ \xs ->
                        error $ unlines $ "Tests form a cycle:" : xs
                    let missing = transitiveClosure follow (map stringyTo res) \\ map stringyTo res
                    when (missing /= []) $
                        error $ unlines $ "Test is a dependency that cannot be reached:" : missing

                    writeFile ".bake.result" $ show $ map stringyTo res
                Just test -> do
                    testAction $ ovenTestInfo oven $ stringyFrom test
        RunExtra{..} -> do
            res <- ovenPatchExtra oven
                (stringyFrom state)
                (fmap stringyFrom $ listToMaybe patch)
            writeFile ".bake.result" $ show res
    where
        getPort p = if p == 0 then snd $ ovenServer oven else p
        getHostPort h p = (if h == "" then fst $ ovenServer oven else h, getPort p)


check :: Stringy s => String -> s -> String -> IO String
check typ _ x = do
    res <- try_ $ evaluate $ force $ stringyTo $ asTypeOf (stringyFrom x) x
    case res of
        Left err -> error $ "Couldn't stringify the " ++ typ ++ " " ++ show x ++ ", got " ++ show err
        Right v -> return v


defaultNames = words "Simon Lennart Dave Brian Warren Joseph Kevin Ralf Paul John Thomas Mark Erik Alastair Colin Philip"
