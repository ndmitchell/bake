{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Define a continuous integration system.
module Development.Bake.Core.Args(
    bake
    ) where

import System.Console.CmdArgs
import Development.Bake.Core.Type hiding (Client)
import Development.Bake.Core.Client
import Development.Bake.Server.Start
import Development.Bake.Core.Send
import Control.Exception.Extra
import General.Extra
import Control.DeepSeq
import System.Directory
import Control.Monad.Extra
import Control.Applicative
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import System.Random
import Paths_bake


data Bake
    = Server {port :: Port, author :: Author, name :: String, timeout :: Double, datadir :: FilePath}
    | Client {host :: Host, port :: Port, author :: Author, name :: String, threads :: Int, ping :: Double}
    | AddPatch {host :: Host, port :: Port, author :: Author, name :: String}
    | DelPatch {host :: Host, port :: Port, author :: Author, name :: String}
    | DelPatches {host :: Host, port :: Port, author :: Author}
    | Pause {host :: Host, port :: Port, author :: Author}
    | Unpause {host :: Host, port :: Port, author :: Author}
      -- actions sent through from Bake itself
    | RunInit
    | RunUpdate {state :: String, patch :: [String]}
    | RunTest {test :: Maybe String, state :: String, patch :: [String]}
    | RunExtra {state :: String, patch :: [String]}
      deriving (Typeable,Data)


bakeMode = cmdArgsMode $ modes
    [Server{port = 0, author = "unknown", name = "", timeout = 5*60, datadir = ""}
    ,Client{host = "", threads = 1, ping = 60}
    ,AddPatch{}
    ,DelPatch{}
    ,DelPatches{}
    ,Pause{}
    ,Unpause{}
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
bake :: Oven state patch test -> IO ()
bake oven@Oven{..} = do
    registerMaster
    x <- cmdArgsRun bakeMode
    case x of
        Server{..} -> do
            datadir <- canonicalizePath =<< if datadir == "" then getDataDir else return datadir
            startServer (getPort port) datadir author name timeout oven
        Client{..} -> do
            name <- if name /= "" then return name else pick defaultNames
            startClient (getHostPort host port) author name threads ping oven
        AddPatch{..} -> sendAddPatch (getHostPort host port) author =<< check "patch" ovenStringyPatch name
        DelPatch{..} -> sendDelPatch (getHostPort host port) author =<< check "patch" ovenStringyPatch name
        DelPatches{..} -> sendDelAllPatches (getHostPort host port) author
        Pause{..} -> sendPause (getHostPort host port) author
        Unpause{..} -> sendUnpause (getHostPort host port) author

        RunInit -> do
            s <- ovenUpdateState Nothing
            writeFile ".bake" $ stringyTo ovenStringyState s
        RunUpdate{..} -> do
            s <- ovenUpdateState $ Just (stringyFrom ovenStringyState state, map (stringyFrom ovenStringyPatch) patch)
            writeFile ".bake" $ stringyTo ovenStringyState s
        RunTest{..} -> do
            case test of
                Nothing -> do
                    let str = stringyTo ovenStringyTest
                    res <- nubOn str <$> ovenPrepare
                        (stringyFrom ovenStringyState state)
                        (map (stringyFrom ovenStringyPatch) patch)

                    -- check the patches all make sense
                    let follow t = map str $ testRequire $ ovenTestInfo $ stringyFrom ovenStringyTest t
                    whenJust (findCycle follow $ map str res) $ \xs ->
                        error $ unlines $ "Tests form a cycle:" : xs

                    xs <- partitionM (testSuitable . ovenTestInfo) res
                    writeFile ".bake" $ show $ both (map str) xs
                Just test -> do
                    testAction $ ovenTestInfo $ stringyFrom ovenStringyTest test
        RunExtra{..} -> do
            res <- ovenPatchExtra
                (stringyFrom ovenStringyState state)
                (fmap (stringyFrom ovenStringyPatch) $ listToMaybe patch)
            writeFile ".bake" $ show res
    where
        getPort p = if p == 0 then snd ovenServer else p
        getHostPort h p = (if h == "" then fst ovenServer else h, getPort p)


check :: String -> Stringy s -> String -> IO String
check typ Stringy{..} x = do
    res <- try_ $ evaluate $ force $ stringyTo $ stringyFrom x
    case res of
        Left err -> error $ "Couldn't stringify the " ++ typ ++ " " ++ show x ++ ", got " ++ show err
        Right v -> return v


defaultNames = words "Simon Lennart Dave Brian Warren Joseph Kevin Ralf Paul John Thomas Mark Erik Alastair Colin Philip"

pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)
