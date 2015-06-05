{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Development.Bake.Core.Message(
    Message(..), Ping(..), Question(..), Answer(..),
    sendMessage, messageToInput, messageFromInput, questionToOutput
    ) where

import Development.Bake.Core.Type
import General.Web
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Aeson hiding (Success)
import System.Time.Extra
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as LBS
import Prelude


data Message
    -- Send by the user
    = AddPatch Author Patch
    | DelPatch Author Patch
    | DelAllPatches Author
    | Requeue Author
    | SetState Author State
    | Pause Author
    | Unpause Author
    | AddSkip Author Test
    | DelSkip Author Test
    -- Sent by the client
    | Pinged Ping
    | Finished {question :: Question, answer :: Answer}
    deriving (Show,Eq)

instance NFData Message where
    rnf (AddPatch x y) = rnf x `seq` rnf y
    rnf (DelPatch x y) = rnf x `seq` rnf y
    rnf (DelAllPatches x) = rnf x
    rnf (Requeue x) = rnf x
    rnf (SetState x y) = rnf x `seq` rnf y
    rnf (Pause x) = rnf x
    rnf (Unpause x) = rnf x
    rnf (AddSkip x y) = rnf x `seq` rnf y
    rnf (DelSkip x y) = rnf x `seq` rnf y
    rnf (Pinged x) = rnf x
    rnf (Finished x y) = rnf x `seq` rnf y

data Question = Question
    {qCandidate :: (State, [Patch])
    ,qTest :: Maybe Test
    ,qThreads :: Int
    ,qClient :: Client
    }
    deriving (Show,Eq,Ord)

instance NFData Question where
    rnf (Question a b c d) = rnf (a,b,c,d)

data Answer = Answer
    {aStdout :: TL.Text
    ,aDuration :: Seconds
    ,aTests :: [Test]
    ,aSuccess :: Bool
    }
    deriving (Show,Eq)

instance NFData Answer where
    rnf (Answer a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

data Ping = Ping
    {pClient :: Client
    ,pAuthor :: Author
    ,pProvide :: [String] -- matches with testRequire
    ,pMaxThreads :: Int
    ,pNowThreads :: Int
    }
    deriving (Show,Eq)

instance NFData Ping where
    rnf (Ping a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

-- JSON instance is only true for Finished
instance ToJSON Message where
    toJSON (Finished q a) = object ["question" .= q, "answer" .= a]
    toJSON _ = error "ToJSON Message is only supported for Finished"

instance FromJSON Message where
    parseJSON (Object v) = Finished <$>
        (v .: "question") <*> (v .: "answer")
    parseJSON _ = mzero

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

toJSONCandidate (s, ps) = object ["state" .= s, "patches" .= ps]

fromJSONCandidate (Object v) = (,) <$> (v .: "state") <*> (v .: "patches")
fromJSONCandidate _ = mzero

instance ToJSON Answer where
    toJSON Answer{..} = object
        ["stdout" .= aStdout
        ,"duration" .= aDuration
        ,"tests" .= aTests
        ,"success" .= aSuccess]

instance FromJSON Answer where
    parseJSON (Object v) = Answer <$>
        (v .: "stdout") <*> (v .: "duration") <*> (v .: "tests") <*> (v .: "success")
    parseJSON _ = mzero


messageToInput :: Message -> Input
messageToInput (AddPatch author (Patch patch)) = Input ["api","add"] [("author",author),("patch",patch)] ""
messageToInput (DelPatch author (Patch patch)) = Input ["api","del"] [("author",author),("patch",patch)] ""
messageToInput (DelAllPatches author) = Input ["api","delall"] [("author",author)] ""
messageToInput (Requeue author) = Input ["api","requeue"] [("author",author)] ""
messageToInput (SetState author (State state)) = Input ["api","set"] [("author",author),("state",state)] ""
messageToInput (Pause author) = Input ["api","pause"] [("author",author)] ""
messageToInput (Unpause author) = Input ["api","unpause"] [("author",author)] ""
messageToInput (AddSkip author (Test test)) = Input ["api","addskip"] [("author",author),("test",test)] ""
messageToInput (DelSkip author (Test test)) = Input ["api","delskip"] [("author",author),("test",test)] ""
messageToInput (Pinged Ping{..}) = Input ["api","ping"] 
    ([("client",fromClient pClient),("author",pAuthor)] ++
     [("provide",x) | x <- pProvide] ++
     [("maxthreads",show pMaxThreads),("nowthreads",show pNowThreads)]) ""
messageToInput x@Finished{} = Input ["api","finish"] [] $ encode x


-- return either an error message (not a valid message), or a message
messageFromInput :: Input -> Either String Message
messageFromInput (Input [msg] args body)
    | msg == "add" = AddPatch <$> str "author" <*> (Patch <$> str "patch")
    | msg == "del" = DelPatch <$> str "author" <*> (Patch <$> str "patch")
    | msg == "delall" = DelAllPatches <$> str "author"
    | msg == "addskip" = AddSkip <$> str "author" <*> (Test <$> str "test")
    | msg == "delskip" = DelSkip <$> str "author" <*> (Test <$> str "test")
    | msg == "requeue" = Requeue <$> str "author"
    | msg == "set" = SetState <$> str "author" <*> (State <$> str "state")
    | msg == "pause" = Pause <$> str "author"
    | msg == "unpause" = Unpause <$> str "author"
    | msg == "ping" = Pinged <$> (Ping <$> (Client <$> str "client") <*>
        str "author" <*> strs "provide" <*> int "maxthreads" <*> int "nowthreads")
    | msg == "finish" = eitherDecode body
    where strs x = Right $ map snd $ filter ((==) x . fst) args
          str x | Just v <- lookup x args = Right v
                | otherwise = Left $ "Missing field " ++ show x ++ " from " ++ show msg
          int x = read <$> str x
messageFromInput (Input msg args body) = Left $ "Invalid API call, got " ++ show msg


questionToOutput :: Maybe Question -> Output
questionToOutput = OutputString . LBS.unpack . encode


sendMessage :: (Host,Port) -> Message -> IO (Maybe Question)
sendMessage hp msg = do
    res <- send hp $ messageToInput msg
    return $ decode res
