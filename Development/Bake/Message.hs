
-- | Define a continuous integration system.
module Development.Bake.Message(
    Message(..), Reply(..), sendMessage, messageFromPayload
    ) where

import Development.Bake.Type
import Development.Bake.Web

data Message
    -- Send by the user
    = AddPatch Author Patch
    | DelPatch Author Patch
    | DelAllPatches Author
    | Pause Author
    | Unpause Author
    -- Sent by the client
    | Ping Author String String [String] Int -- name, cookie, provides, threads
    | Finished (Candidate State Patch) String (Maybe Test) String Double (Either Int [Test])
                                       -- stdout time   result

data Reply = Reply (Candidate State Patch) (Maybe Test)

messageToPayload :: Message -> Payload
messageToPayload = error "messageToPayload"


messageFromPayload :: Payload -> Message
messageFromPayload = error "messageFromPayload"


sendMessage :: (Host,Port) -> Message -> IO [Reply]
sendMessage hp msg = do
    send hp $ messageToPayload msg
    return []
