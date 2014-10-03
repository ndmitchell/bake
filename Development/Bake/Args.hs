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
    | Run {output :: FilePath, test :: String, state :: String, patch :: [String]}
      deriving (Typeable,Data)


bakeMode = cmdArgsMode $ modes
    [Server{port = 80, author = "unknown", name = "", timeout = 5*60}
    ,Client{server = "", threads = 0, ping = 60}
    ,AddPatch{}
    ,DelPatch{}
    ,DelPatches{}
    ,Pause{}
    ,Unpause{}
    ,Run "" "" "" []
    ]

bake :: Oven state patch test -> IO ()
bake oven = do
    x <- cmdArgsRun bakeMode
    case x of
        Server{..} -> startServer port author name timeout oven
        Client{..} -> startClient (hp server) author name threads ping oven
        AddPatch{..} -> sendAddPatch (hp server) author =<< checkPatch oven name
        DelPatch{..} -> sendDelPatch (hp server) author =<< checkPatch oven name
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


checkPatch :: Oven state patch test -> String -> IO String
checkPatch Oven{ovenStringyPatch=Stringy{..}} x = do
    res <- try_ $ evaluate $ force $ stringyTo $ stringyFrom x
    case res of
        Left err -> error $ "Couldn't stringify the patch " ++ show x ++ ", got " ++ show err
        Right v -> return v


