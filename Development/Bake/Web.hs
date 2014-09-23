
module Development.Bake.Web(
    Payload(..), send, server
    ) where

import Development.Bake.Type


data Payload = Payload
    {payloadURL :: String
    ,payloadArgs :: [(String, String)]
    ,payloadBody :: String
    }

send :: (Host,Port) -> Payload -> IO String
send = undefined


server :: Port -> (Payload -> IO (Either FilePath String)) -> IO ()
server = undefined
