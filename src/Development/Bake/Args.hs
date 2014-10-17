{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Define a continuous integration system.
module Development.Bake.Args(
    bake
    ) where

import System.Console.CmdArgs
import Development.Bake.Type hiding (Client)
import Development.Bake.Client
import Development.Bake.Server.Start
import Development.Bake.Send
import Control.Exception.Extra
import Control.DeepSeq
import System.Directory
import Control.Monad.Extra
import Data.Maybe
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
    | RunTest {output :: FilePath, test :: Maybe String, state :: String, patch :: [String]}
    | RunExtra {output :: FilePath, state :: String, patch :: [String]}
      deriving (Typeable,Data)


bakeMode = cmdArgsMode $ modes
    [Server{port = 0, author = "unknown", name = "", timeout = 5*60, datadir = ""}
    ,Client{host = "", threads = 1, ping = 60}
    ,AddPatch{}
    ,DelPatch{}
    ,DelPatches{}
    ,Pause{}
    ,Unpause{}
    ,RunTest "" Nothing "" []
    ,RunExtra "" "" []
    ] &= verbosity

-- | The entry point to the system. Usually you will define:
--
-- > main = bake myOven
--
--   Where @myOven@ defines details about the server. The program
--   deals with command line arguments, run @--help@ for details.
bake :: Oven state patch test -> IO ()
bake oven@Oven{..} = do
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
        RunTest{..} -> do
            case test of
                Nothing -> do
                    res <- ovenPrepare
                        (stringyFrom ovenStringyState state)
                        (map (stringyFrom ovenStringyPatch) patch)
                    (yes,no) <- partitionM (testSuitable . ovenTestInfo) res
                    let op = map (stringyTo ovenStringyTest)
                    writeFile output $ show (op yes, op no)
                Just test -> do
                    testAction $ ovenTestInfo $ stringyFrom ovenStringyTest test
        RunExtra{..} -> do
            res <- ovenPatchExtra
                (stringyFrom ovenStringyState state)
                (fmap (stringyFrom ovenStringyPatch) $ listToMaybe patch)
            writeFile output $ show res
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
