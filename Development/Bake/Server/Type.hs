
-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..), defaultServer, History(..), Running(..)
    ) where

import Development.Bake.Type

defaultServer :: State -> Server
defaultServer s = Server [] [] [] (Candidate s []) [] Nothing

data Server = Server
    {history :: [History]
    ,running :: [Running]
    ,alias :: [(State, Candidate State Patch)]
    ,active :: Candidate State Patch
    ,blacklist :: [String] -- people who failed to return
    ,paused :: Maybe [Patch]
    } deriving Show

data History = History (Candidate State Patch) (Maybe Test) FilePath Double (Either Int [Test])
    deriving Show

data Running = Running (Candidate State Patch) (Maybe Test) Double String
    deriving Show
