{-# LANGUAGE ScopedTypeVariables #-}

module Development.Bake.Web(
    Input(..), Output(..), send, server
    ) where

import Development.Bake.Type hiding (run)
import Network.Wai.Handler.Warp hiding (Port)
import Network.Wai
import Control.Exception
import Network.HTTP.Types.Status
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


data Input = Input
    {inputURL :: [String]
    ,inputArgs :: [(String, String)]
    ,inputBody :: String
    }

data Output
    = OutputString String
    | OutputFile FilePath
    | OutputError String
    | OutputMissing


send :: (Host,Port) -> Input -> IO String
send = error "send"


server :: Port -> (Input -> IO Output) -> IO ()
server port act = runSettings (setOnException exception $ setPort port defaultSettings) $ \req reply -> do
    bod <- strictRequestBody req
    let pay = Input
            (map Text.unpack $ pathInfo req)
            [(BS.unpack a, maybe "" BS.unpack b) | (a,b) <- queryString req]
            (LBS.unpack bod)
    res <- act pay
    reply $ case res of
        OutputFile file -> responseFile status200 [] file Nothing
        OutputString msg -> responseLBS status200 [] $ LBS.pack msg
        OutputError msg -> responseLBS status500 [] $ LBS.pack msg
        OutputMissing -> responseLBS status404 [] $ LBS.pack "Resource not found"

exception :: Maybe Request -> SomeException -> IO ()
exception r e
    | Just (_ :: InvalidRequest) <- fromException e = return ()
    | otherwise = putStrLn $ "Error when processing " ++ maybe "Nothing" (show . rawPathInfo) r ++
                             "\n    " ++ show e
