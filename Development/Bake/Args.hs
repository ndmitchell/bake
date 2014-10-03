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
import Development.Bake.Util
import Control.Exception
import Control.DeepSeq


type HostPort = String

data Bake
    = Server {port :: Port, author :: Author, name :: String, timeout :: Double}
    | Client {server :: HostPort, author :: Author, name :: String, threads :: Int, ping :: Double}
    | AddPatch {server :: HostPort, author :: Author, name :: String}
    | DelPatch {server :: HostPort, author :: Author, name :: String}
    | DelPatches {server :: HostPort, author :: Author}
    | Pause {server :: HostPort, author :: Author}
    | Unpause {server :: HostPort, author :: Author}
    | Run {output :: FilePath, test :: Maybe String, state :: String, patch :: [String]}
      deriving (Typeable,Data)


bakeMode = cmdArgsMode $ modes
    [Server{port = 80, author = "unknown", name = "", timeout = 5*60}
    ,Client{server = "", threads = 1, ping = 60}
    ,AddPatch{}
    ,DelPatch{}
    ,DelPatches{}
    ,Pause{}
    ,Unpause{}
    ,Run "" Nothing "" []
    ]

bake :: Oven state patch test -> IO ()
bake oven@Oven{..} = do
    x <- cmdArgsRun bakeMode
    case x of
        Server{..} -> startServer port author name timeout oven
        Client{..} -> startClient (hp server) author name threads ping oven
        AddPatch{..} -> sendAddPatch (hp server) author =<< check "patch" ovenStringyPatch name
        DelPatch{..} -> sendDelPatch (hp server) author =<< check "patch" ovenStringyPatch name
        DelPatches{..} -> sendDelAllPatches (hp server) author
        Pause{..} -> sendPause (hp server) author
        Unpause{..} -> sendUnpause (hp server) author
        Run{..} -> do
            case test of
                Nothing -> do
                    res <- ovenPrepare $ Candidate
                        (stringyFrom ovenStringyState state)
                        (map (stringyFrom ovenStringyPatch) patch)
                    writeFile output $ unlines $ map (stringyTo ovenStringyTest) res
                Just test -> do
                    testAction $ ovenTestInfo $ stringyFrom ovenStringyTest test
    where
        hp "" = ovenDefaultServer
        hp s = (h, read $ drop 1 p)
            where (h,p) = break (== ':') s


check :: String -> Stringy s -> String -> IO String
check typ Stringy{..} x = do
    res <- try_ $ evaluate $ force $ stringyTo $ stringyFrom x
    case res of
        Left err -> error $ "Couldn't stringify the " ++ typ ++ " " ++ show x ++ ", got " ++ show err
        Right v -> return v


