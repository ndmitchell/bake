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


type HostPort = String

data Bake
    = Server {port :: Port, author :: Author, name :: String}
    | Client {server :: HostPort, author :: Author, name :: String, threads :: Int, provide :: [String]}
    | AddPatch {server :: HostPort, author :: Author, name :: String}
    | DelPatch {server :: HostPort, author :: Author, name :: String}
    | DelPatches {server :: HostPort, author :: Author}
    | Pause {server :: HostPort, author :: Author}
    | Unpause {server :: HostPort, author :: Author}
    | Run {output :: FilePath, test :: String, state :: String, patch :: [String]}
      deriving (Typeable,Data)


bakeMode = cmdArgsMode $ modes
    [Server{port = 80, author = "unknown", name = ""}
    ,Client{server = "", threads = 0, provide = []}
    ,AddPatch{}
    ,DelPatch{}
    ,DelPatches{}
    ,Pause{}
    ,Unpause{}
    ,Run "" "" "" []
    ]

bake :: (Show state, Read state, Show patch, Read patch, Show test, Read test)
     => Oven state patch test -> IO ()
bake oven = do
    x <- cmdArgsRun bakeMode
    case x of
        Server{..} -> startServer port author name oven
        Client{..} -> startClient (hp server) author name provide threads
        AddPatch{..} -> sendAddPatch (hp server) author name
        DelPatch{..} -> sendDelPatch (hp server) author name
        DelPatches{..} -> sendDelAllPatches (hp server) author
        Pause{..} -> sendPause (hp server) author
        Unpause{..} -> sendUnpause (hp server) author
        Run{..} -> do
            -- FIXME: Should wrap the oven so all tests become run in a separate process
            -- and then use that up above
            error "bake.run"
            -- let TestInfo{..} = ovenRunTest oven (Candidate (read state) (map read patch)) (read test)
            -- writeFile output . show =<< testAction
    where
        hp "" = ovenDefaultServer oven
        hp s = (h, read $ drop 1 p)
            where (h,p) = break (== ':') s
