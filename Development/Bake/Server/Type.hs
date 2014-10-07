
-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..), defaultServer,
    Question(..), Answer(..), Ping(..)
    ) where

import Development.Bake.Type
import Development.Bake.Message
import Data.Time.Clock


defaultServer :: State -> Server
defaultServer s = Server [] [] [] (Candidate s []) Nothing []

data Server = Server
    {history :: [(UTCTime, Question, Maybe Answer)]
        -- ^ Questions you have sent to clients, and how they responded (if they have).
        --   The aStdout has been written to disk, and the value is a filename containing the stdout.
    ,updates :: [(State, Candidate State Patch)]
        -- ^ Updates that have been made
    ,pings :: [(UTCTime, Ping)]
        -- ^ Latest time of a ping sent by each client
    ,active :: Candidate State Patch
        -- ^ The candidate we are currently aiming to prove
    ,paused :: Maybe [Patch]
        -- ^ 'Just' if we are paused, and the number of people queued up
    ,authors :: [(Maybe Patch, Author)]
        -- ^ Authors associated with each patch (Nothing is the server author)
    } deriving Show
