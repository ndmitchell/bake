{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Development.Bake.Core.Message(
    Message(..), Ping(..), Question(..), Answer(..),
    sendMessage, messageToInput, messageFromInput, questionToOutput
    ) where

import Development.Bake.Core.Type
import General.Web
import General.BigString
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Aeson hiding (Success)
import System.Time.Extra
import Safe
import qualified Data.ByteString.Lazy.Char8 as LBS
import Prelude


data Message
    -- Send by the user
    = SetState Author State
    | AddPatch Author Patch
    | DelPatch Patch
    | Requeue
    | Pause
    | Unpause
    | AddSkip Author Test
    | DelSkip Test
    -- Sent by the client
    | Pinged Ping
    | Finished {question :: Question, answer :: Answer}
    deriving Show

instance NFData Message where
    rnf (AddPatch x y) = rnf x `seq` rnf y
    rnf (DelPatch x) = rnf x
    rnf Requeue = ()
    rnf (SetState x y) = rnf x `seq` rnf y
    rnf Pause = ()
    rnf Unpause = ()
    rnf (AddSkip x y) = rnf x `seq` rnf y
    rnf (DelSkip x) = rnf x
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
    {aStdout :: BigString
    ,aDuration :: Maybe Seconds -- Nothing for a skip
    ,aTests :: [Test]
    ,aSuccess :: Bool
    }
    deriving Show

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


messageToInput :: Message -> Input
messageToInput (AddPatch author patch) = Input ["api","add"] [("author",author),("patch",fromPatch patch)] []
messageToInput (DelPatch patch) = Input ["api","del"] [("patch",fromPatch patch)] []
messageToInput Requeue = Input ["api","requeue"] [] []
messageToInput (SetState author state) = Input ["api","set"] [("author",author),("state",fromState state)] []
messageToInput Pause = Input ["api","pause"] [] []
messageToInput Unpause = Input ["api","unpause"] [] []
messageToInput (AddSkip author test) = Input ["api","addskip"] [("author",author),("test",fromTest test)] []
messageToInput (DelSkip test) = Input ["api","delskip"] [("test",fromTest test)] []
messageToInput (Pinged Ping{..}) = Input ["api","ping"] 
    ([("client",fromClient pClient),("author",pAuthor)] ++
     [("provide",x) | x <- pProvide] ++
     [("maxthreads",show pMaxThreads),("nowthreads",show pNowThreads)]) []
messageToInput (Finished Question{..} Answer{..}) = Input ["api","finish"] []
    [("state", bigStringFromString $ fromState $ fst qCandidate)
    ,("patch", bigStringFromString $ unlines $ map fromPatch $ snd qCandidate)
    ,("test", bigStringFromString $ maybe "" fromTest qTest)
    ,("threads", bigStringFromString $ show qThreads)
    ,("client", bigStringFromString $ fromClient qClient)
    ,("stdout", aStdout)
    ,("duration", bigStringFromString $ maybe "" show aDuration)
    ,("tests", bigStringFromString $ unlines $ map fromTest aTests)
    ,("success", bigStringFromString $ show aSuccess)]


-- return either an error message (not a valid message), or a message
messageFromInput :: Input -> Either String Message
messageFromInput (Input [msg] args body)
    | msg == "add" = AddPatch <$> str "author" <*> (toPatch <$> str "patch")
    | msg == "del" = DelPatch <$> (toPatch <$> str "patch")
    | msg == "addskip" = AddSkip <$> str "author" <*> (toTest <$> str "test")
    | msg == "delskip" = DelSkip <$> (toTest <$> str "test")
    | msg == "requeue" = pure Requeue
    | msg == "set" = SetState <$> str "author" <*> (toState <$> str "state")
    | msg == "pause" = pure Pause
    | msg == "unpause" = pure Unpause
    | msg == "ping" = Pinged <$> (Ping <$> (toClient <$> str "client") <*>
        str "author" <*> strs "provide" <*> int "maxthreads" <*> int "nowthreads")
    where strs x = Right $ map snd $ filter ((==) x . fst) args
          str x | Just v <- lookup x args = Right v
                | otherwise = Left $ "Missing field " ++ show x ++ " from " ++ show msg
          int x = readNote "messageFromInput, expecting Int" <$> str x
messageFromInput (Input [msg] args body)
    | msg == "finish" = do
        let f x = case lookup x body of Nothing -> Left $ "Missing field " ++ show x ++ " from " ++ show (map fst body); Just x -> Right x
        state <- toState . bigStringToString <$> f "state"
        patch <- map toPatch . lines . bigStringToString <$> f "patch"
        qTest <- (\x -> if null x then Nothing else Just $ toTest x) . bigStringToString <$> f "test"
        qThreads <- read . bigStringToString <$> f "threads"
        qClient <- toClient . bigStringToString <$> f "client"
        aStdout <- f "stdout"
        aDuration <- (\x -> if null x then Nothing else Just $ read x) . bigStringToString <$> f "duration"
        aTests <- map toTest . lines . bigStringToString <$> f "tests"
        aSuccess <- read . bigStringToString <$> f "success"
        return $ Finished Question{qCandidate=(state,patch),..} Answer{..}
messageFromInput (Input msg args body) = Left $ "Invalid API call, got " ++ show msg


questionToOutput :: Maybe Question -> Output
questionToOutput = OutputString . LBS.unpack . encode


sendMessage :: (Host,Port) -> Message -> IO (Maybe Question)
sendMessage hp msg = do
    res <- send hp $ messageToInput msg
    return $ decode res
