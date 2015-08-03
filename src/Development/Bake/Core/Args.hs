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
import System.IO.Extra
import System.Directory.Extra
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
    | DelPatch {host :: Host, port :: Port, name :: String}
    | Requeue {host :: Host, port :: Port}
    | SetState {host :: Host, port :: Port, author :: [Author], state :: String}
    | Pause {host :: Host, port :: Port}
    | Unpause {host :: Host, port :: Port}
    | GC {bytes :: Integer, ratio :: Double, days :: Double, dirs :: [FilePath]}
    | Admin {password :: [String]}
    | View {port :: Port, file :: FilePath}
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
    ,Requeue{}
    ,SetState{state = ""}
    ,Pause{}
    ,Unpause{}
    ,GC 0 0 7 ([] &= args)
    ,Admin ([] &= args)
    ,View{file = "" &= args}
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
        Server{..} -> startServer (getPort port) author timeout admin False oven
        View{..} -> do
            when (file == "") $ error "You must pass a file"
            file <- canonicalizePath file
            withTempDir $ \dir -> withCurrentDirectory dir $ do
                createDirectoryIfMissing True $ dir </> "bake-store"
                copyFile file $ dir </> "bake-store" </> "bake.sqlite"
                -- the concrete ensures nothing ever results in a parse error
                startServer (getPort port) [] 100 "" True $ snd $ concrete oven
        Client{..} -> do
            name <- if name /= "" then return name else pick defaultNames
            startClient (getHostPort host port) author1 name threads provide ping oven
        AddPatch{..} -> sendAddPatch (getHostPort host port) author1 =<< check "patch" (undefined :: patch) name
        DelPatch{..} -> sendDelPatch (getHostPort host port) =<< check "patch" (undefined :: patch) name
        Requeue{..} -> sendRequeue (getHostPort host port)
        SetState{..} -> sendSetState (getHostPort host port) author1 state
        Pause{..} -> sendPause (getHostPort host port)
        Unpause{..} -> sendUnpause (getHostPort host port)
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

                    case validTests (ovenTestInfo oven) res of
                        Left err -> fail err
                        Right () -> return ()

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
