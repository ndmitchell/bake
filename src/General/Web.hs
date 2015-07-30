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

-- S for server, C for client
import Development.Bake.Core.Type hiding (run)
import Network.Wai as S
import Network.Wai.Parse
import Data.Function
import General.Extra
import General.BigString
import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.IO
import Network.HTTP.Conduit as C
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types.Status
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Console.CmdArgs.Verbosity


data Input = Input
    {inputURL :: [String]
    ,inputArgs :: [(String, String)]
    ,inputBody :: [(String, BigString)]
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
    whenLoud $ print ("sending",length inputBody,host,port)
    req <- parseUrl url
    m <- newManager conduitManagerSettings
    withs (map (withTmpFile . snd) inputBody) $ \files -> do
        body <- formDataBody (zipWith partFileSourceChunked (map (Text.pack . fst) inputBody) files) req
        responseBody <$> httpLbs body m


server :: Port -> (Input -> IO Output) -> IO ()
#ifdef PROFILE
server port act = return ()
#else
server port act = runSettings settings $ \req reply -> do
    whenLoud $ print ("receiving", map Text.unpack $ pathInfo req, S.requestHeaders req, port)
    (_, files) <- parseRequestBody backEndBigString req
    let pay = Input
            (map Text.unpack $ pathInfo req)
            [(BS.unpack a, maybe "" BS.unpack b) | (a,b) <- S.queryString req]
            [(BS.unpack name, fileContent) | (name, FileInfo{..}) <- files]
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
    where
        settings = setOnExceptionResponse exceptionResponseForDebug $
                   setOnException exception $
                   setPort port defaultSettings


backEndBigString :: BackEnd BigString
backEndBigString _ _ ask = do
    tmp <- newTmpFile
    withTmpFile tmp $ \file -> do
        withFile file AppendMode $ \h -> do
            fix $ \loop -> do
                bs <- ask
                unless (BS.null bs) $ do
                    BS.hPut h bs
                    loop
    return tmp


exception :: Maybe S.Request -> SomeException -> IO ()
exception r e = when (defaultShouldDisplayException e) $
    hPutStrLn stderr $ "Error when processing " ++ maybe "Nothing" (show . rawPathInfo) r ++ "\n    " ++ show e
#endif
