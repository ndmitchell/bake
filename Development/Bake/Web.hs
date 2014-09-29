{-# LANGUAGE ScopedTypeVariables #-}

module Development.Bake.Web(
    Payload(..), send, server
    ) where

import Development.Bake.Type hiding (run)
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai
import Control.Exception
import Network.HTTP.Types.Status
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


data Payload = Payload
    {payloadURL :: [String]
    ,payloadArgs :: [(String, String)]
    ,payloadBody :: String
    }

send :: (Host,Port) -> Payload -> IO String
send = error "send"


server :: Port -> (Payload -> IO (Either FilePath String)) -> IO ()
server port act = runSettings (setOnException exception $ setPort port defaultSettings) $ \req reply -> do
    bod <- strictRequestBody req
    let pay = Payload
            (map Text.unpack $ pathInfo req)
            [(BS.unpack a, maybe "" BS.unpack b) | (a,b) <- queryString req]
            (LBS.unpack bod)
    res <- act pay
    reply $ case res of
        Left file -> responseFile status200 [] file Nothing
        Right msg -> responseLBS status200 [] $ LBS.pack msg

exception :: Maybe Request -> SomeException -> IO ()
exception r e
    | Just (_ :: InvalidRequest) <- fromException e = return ()
    | otherwise = putStrLn $ "Error when processing " ++ maybe "Nothing" (show . rawPathInfo) r ++
                             "\n    " ++ show e
