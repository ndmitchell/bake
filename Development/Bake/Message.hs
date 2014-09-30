
-- | Define a continuous integration system.
module Development.Bake.Message(
    Message(..), Question(..), Answer(..), Ping(..),
    sendMessage, messageFromInput, questionsToOutput
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Data.Time.Clock


data Message
    -- Send by the user
    = AddPatch Author Patch
    | DelPatch Author Patch
    | DelAllPatches Author
    | Pause Author
    | Unpause Author
    -- Sent by the client
    | Pinged Ping
    | Finished Question Answer
    deriving (Show,Eq)


data Question = Question
    {qCandidate :: Candidate State Patch
    ,qTest :: Maybe Test
    ,qThreads :: Int
    ,qStarted :: UTCTime
    ,qClient :: Client
    }
    deriving (Show,Eq)

data Answer = Answer
    {aStdout :: String
    ,aDuration :: Double
    ,aReply :: Either Int [Test]
    }
    deriving (Show,Eq)

data Ping = Ping
    {pClient :: Client
    ,pAuthor :: Author
    ,pName :: String
    ,pThreads :: Int
    }
    deriving (Show,Eq)


messageToInput :: Message -> Input
messageToInput m = error $ show ("messageToInput",m)


-- return either an error message (not a valid message), or a message
messageFromInput :: Input -> Either String Message
messageFromInput i = error $ show ("messageFromInput",i)

questionsToOutput :: [Question] -> Output
questionsToOutput qs = error $ show ("questionToOutput",qs)


sendMessage :: (Host,Port) -> Message -> IO [Question]
sendMessage hp msg = do
    send hp $ messageToInput msg
    return []
