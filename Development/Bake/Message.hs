{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Development.Bake.Message(
    Message(..), Ping(..), Question(..), Answer(..), Status(..),
    sendMessage, messageFromInput, questionToOutput
    ) where

import Development.Bake.Type
import Development.Bake.Web
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS


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
    ,qClient :: Client
    }
    deriving (Show,Eq)

instance ToJSON Question where
    toJSON Question{..} = object
        ["candidate" .= toJSONCandidate qCandidate
        ,"test" .= qTest
        ,"threads" .= qThreads
        ,"client" .= qClient]

instance FromJSON Question where
    parseJSON (Object v) = Question <$>
        (fromJSONCandidate =<< (v .: "candidate")) <*> (v .: "test") <*> (v .: "threads") <*> (v .: "client")
    parseJSON _ = mzero

toJSONCandidate (Candidate s ps) = object ["state" .= s, "patches" .= ps]

fromJSONCandidate (Object v) = Candidate <$> (v .: "state") <*> (v .: "patches")
fromJSONCandidate _ = mzero

data Answer = Answer
    {aStdout :: String
    ,aDuration :: Double
    ,aTests :: [Test] -- only filled in if qTest is Nothing
    ,aStatus :: Status
    }
    deriving (Show,Eq)

data Status = Success | Failure | NotApplicable deriving (Show,Eq)

data Ping = Ping
    {pClient :: Client
    ,pAuthor :: Author
    ,pMaxThreads :: Int
    ,pNowThreads :: Int
    }
    deriving (Show,Eq)


messageToInput :: Message -> Input
messageToInput (AddPatch author (Patch patch)) = Input ["api","add"] [("author",author),("patch",patch)] ""
messageToInput (DelPatch author (Patch patch)) = Input ["api","del"] [("author",author),("patch",patch)] ""
messageToInput (DelAllPatches author) = Input ["api","delall"] [("author",author)] ""
messageToInput (Pause author) = Input ["api","pause"] [("author",author)] ""
messageToInput (Unpause author) = Input ["api","unpause"] [("author",author)] ""
messageToInput (Pinged Ping{..}) = Input ["api","ping"]
    [("client",fromClient pClient),("author",pAuthor)
    ,("maxthreads",show pMaxThreads),("nowthreads",show pNowThreads)] ""
messageToInput (Finished Question{..} Answer{..}) = error $ "messageToInput Finished: " ++ show (Finished Question{..} Answer{..})


-- return either an error message (not a valid message), or a message
messageFromInput :: Input -> Either String Message
messageFromInput (Input [msg] args body)
    | msg == "add" = AddPatch <$> str "author" <*> (Patch <$> str "patch")
    | msg == "del" = DelPatch <$> str "author" <*> (Patch <$> str "patch")
    | msg == "delall" = DelAllPatches <$> str "author"
    | msg == "pause" = Pause <$> str "author"
    | msg == "ping" = Pinged <$> (Ping <$> (Client <$> str "client") <*>
        str "author" <*> int "maxthreads" <*> int "nowthreads")
    | msg == "finish" = error "messageFromInput finish"
    where str x | Just v <- lookup x args = Right v
                | otherwise = Left $ "Missing field " ++ show x ++ " from " ++ show msg
          int x = read <$> str x
messageFromInput (Input msg args body) = Left $ "Invalid API call, got " ++ show msg


questionToOutput :: Maybe Question -> Output
questionToOutput = OutputString . LBS.unpack . encode



sendMessage :: (Host,Port) -> Message -> IO (Maybe Question)
sendMessage hp msg = do
    res <- send hp $ messageToInput msg
    return $ decode $ LBS.pack res
