
-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..), defaultServer,
    Question(..), Answer(..), Ping(..)
    ) where

import Development.Bake.Type
import Development.Bake.Message


defaultServer :: State -> Server
defaultServer s = Server [] [] [] (Candidate s []) Nothing

data Server = Server
    {history :: [(Question, Maybe Answer)]
        -- ^ Questions you have sent to clients, and how they responded (if they have).
        --   The aStdout has been written to disk, and the value is a filename containing the stdout.
    ,pings :: [Ping]
        -- ^ Latest time of a ping sent by each client
    ,alias :: [(State, Candidate State Patch)]
        -- ^ Updates perform since we started running
    ,active :: Candidate State Patch
        -- ^ The candidate we are currently aiming to prove
    ,paused :: Maybe [Patch]
        -- ^ 'Just' if we are paused, and the number of people queued up
    } deriving Show
