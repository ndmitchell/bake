{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Message(
    Message(..), Question(..), Answer(..), Ping(..),
    sendMessage, messageFromInput, questionsToOutput
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Data.Time.Clock
import Control.Applicative


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
messageToInput (AddPatch author (Patch patch)) = Input ["api","add"] [("author",author),("patch",patch)] ""
messageToInput (DelPatch author (Patch patch)) = Input ["api","del"] [("author",author),("patch",patch)] ""
messageToInput (DelAllPatches author) = Input ["api","delall"] [("author",author)] ""
messageToInput (Pause author) = Input ["api","pause"] [("author",author)] ""
messageToInput (Unpause author) = Input ["api","unpause"] [("author",author)] ""
messageToInput (Pinged Ping{..}) = Input ["api","ping"]
    [("client",fromClient pClient),("author",pAuthor),("name",pName),("threads",show pThreads)] ""
messageToInput (Finished Question{..} Answer{..}) = error "messageToInput Finished"


-- return either an error message (not a valid message), or a message
messageFromInput :: Input -> Either String Message
messageFromInput (Input [msg] args body)
    | msg == "add" = AddPatch <$> str "author" <*> (Patch <$> str "patch")
    | msg == "del" = DelPatch <$> str "author" <*> (Patch <$> str "patch")
    | msg == "delall" = DelAllPatches <$> str "author"
    | msg == "pause" = Pause <$> str "author"
    | msg == "ping" = Pinged <$> (Ping <$> (Client <$> str "client") <*> str "author" <*> str "name" <*> int "threads")
    | msg == "finish" = error "messageFromInput finish"
    where str x | Just v <- lookup x args = Right v
                | otherwise = Left $ "Missing field " ++ show x ++ " from " ++ show msg
          int x = read <$> str x
messageFromInput (Input msg args body) = Left $ "Invalid API call, got " ++ show msg


questionsToOutput :: [Question] -> Output
questionsToOutput qs = error $ show ("questionsToOutput",qs)


sendMessage :: (Host,Port) -> Message -> IO [Question]
sendMessage hp msg = do
    send hp $ messageToInput msg
    return []
