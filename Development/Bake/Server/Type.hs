
-- | Define a continuous integration system.
module Development.Bake.Server.Type(
    Server(..), defaultServer,
    Question(..), Answer(..), Ping(..)
    ) where

import Development.Bake.Type
import Development.Bake.Message
import Data.Time.Clock


defaultServer :: State -> Server
defaultServer s = Server [] [] [] (s,[]) Nothing [] [] []

data Server = Server
    {history :: [(UTCTime, Question, Maybe Answer)]
        -- ^ Questions you have sent to clients, and how they responded (if they have).
        --   The aStdout has been written to disk, and the value is a filename containing the stdout.
    ,updates :: [(UTCTime, State, (State, [Patch]))]
        -- ^ Updates that have been made
    ,pings :: [(UTCTime, Ping)]
        -- ^ Latest time of a ping sent by each client
    ,active :: (State, [Patch])
        -- ^ The candidate we are currently aiming to prove
    ,paused :: Maybe [(UTCTime, Patch)]
        -- ^ 'Just' if we are paused, and the number of people queued up
    ,submitted :: [(UTCTime, Patch)]
        -- ^ List of all patches that have been submitted over time
    ,authors :: [(Maybe Patch, Author)]
        -- ^ Authors associated with each patch (Nothing is the server author)
    ,extra :: [(Patch, (String, String))]
        -- ^ Extra information that was computed for each string (cached forever)
    } deriving Show
