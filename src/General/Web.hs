{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings, CPP #-}

module General.Web(
    Input(..), Output(..), send, server
    ) where

-- #define PROFILE

-- For some reason, profiling stops working if I import warp
-- Tracked as https://github.com/yesodweb/wai/issues/311
#ifndef PROFILE
import Network.Wai.Handler.Warp hiding (Port)
#endif

import Development.Bake.Core.Type hiding (run)
import Network.Wai
import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.IO
import Network.HTTP.Types.Status
import Network.HTTP hiding (Request)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Console.CmdArgs.Verbosity


data Input = Input
    {inputURL :: [String]
    ,inputArgs :: [(String, String)]
    ,inputBody :: LBS.ByteString
    } deriving Show

data Output
    = OutputString String
    | OutputHTML String
    | OutputFile FilePath
    | OutputError String
    | OutputMissing
      deriving Show

instance NFData Output where
    rnf (OutputString x) = rnf x
    rnf (OutputHTML x) = rnf x
    rnf (OutputFile x) = rnf x
    rnf (OutputError x) = rnf x
    rnf OutputMissing = ()


send :: (Host,Port) -> Input -> IO LBS.ByteString
send (host,port) Input{..} = do
    let url = "http://" ++ host ++ ":" ++ show port ++ concatMap ('/':) inputURL ++
              concat (zipWith (++) ("?":repeat "&") [a ++ "=" ++ b | (a,b) <- inputArgs])
    whenLoud $ print ("sending",inputBody,host,port)
    res <- simpleHTTP (getRequest url)
        {rqBody=inputBody
        ,rqHeaders=[Header HdrContentType "application/x-www-form-urlencoded", Header HdrContentLength $ show $ LBS.length inputBody]}
    case res of
        Left err -> error $ show err
        Right r | rspCode r /= (2,0,0) -> error $
                    "Incorrect code: " ++ show (rspCode r,rspReason r,url) ++ "\n" ++ show (rspBody r)
                | otherwise -> return $ rspBody r


server :: Port -> (Input -> IO Output) -> IO ()
#ifdef PROFILE
server port act = return ()
#else
server port act = runSettings (setOnException exception $ setPort port defaultSettings) $ \req reply -> do
    bod <- strictRequestBody req
    whenLoud $ print ("receiving",bod,requestHeaders req,port)
    let pay = Input
            (map Text.unpack $ pathInfo req)
            [(BS.unpack a, maybe "" BS.unpack b) | (a,b) <- queryString req]
            bod
    res <- act pay
    -- from http://stackoverflow.com/questions/49547/making-sure-a-web-page-is-not-cached-across-all-browsers
    let nocache = [("Cache-Control","no-cache, no-store, must-revalidate")
                  ,("Pragma","no-cache")
                  ,("Expires","0")]
    reply $ case res of
        OutputFile file -> responseFile status200 nocache file Nothing
        OutputString msg -> responseLBS status200 nocache $ LBS.pack msg
        OutputHTML msg -> responseLBS status200 (("content-type","text/html"):nocache) $ LBS.pack msg
        OutputError msg -> responseLBS status500 nocache $ LBS.pack msg
        OutputMissing -> responseLBS status404 nocache $ LBS.pack "Resource not found"

exception :: Maybe Request -> SomeException -> IO ()
exception r e = when (defaultShouldDisplayException e) $
    hPutStrLn stderr $ "Error when processing " ++ maybe "Nothing" (show . rawPathInfo) r ++ "\n    " ++ show e
#endif
