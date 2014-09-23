
-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..), defaultServer, History(..), Running(..)
    ) where

import Development.Bake.Type

defaultServer :: State -> Server
defaultServer s = Server [] [] [] (Candidate s [])

data Server = Server
    {history :: [History]
    ,running :: [Running]
    ,alias :: [(State, Candidate State Patch)]
    ,active :: Candidate State Patch
    }

data History = History (Candidate State Patch) (Maybe Test) FilePath Double (Either Int [TestInfo Test])

data Running = Running (Candidate State Patch) (Maybe Test) Double String



{-
do
    process $ \    = AddPatch Author Patch
    | DelPatch Author Patch
    | DelAllPatches Author
    | Pause Author
    | Unpause Author
    -- Sent by the client
    | Ping Author String String Int -- name, cookie, threads
    | Finished (Candidate State Patch) (Maybe Test) String Double (Either Int [TestInfo Test])


newtype Sever = Server
    {history :: [History]
    ,alias :: [(State, Candidate State Patch)]
    ,current :: Candidate State Patch
    ,active :: [()]
    }

-}
