
module Development.Bake.Email(ovenNotifyEmail) where

import Development.Bake.Core.Type
import Network.Mail.SMTP
import Data.String


-- | Email notifications when users should be notified about success/failure.
--   Requires the host/port of an SMTP server. Not yet implemented, will always crash.
ovenNotifyEmail :: (Host,Port) -> Oven state patch test -> Oven state patch test
ovenNotifyEmail hp o = o{ovenNotify = \a s -> sendEmail hp a s >> ovenNotify o a s}

sendEmail :: (Host,Port) -> [Author] -> String -> IO ()
sendEmail (h,p) to msg = sendMail' h (fromIntegral p) email
    where email = simpleMail (addr "bake@example.com") (map addr to) [] [] (fromString "Bake notification") [plainTextPart $ fromString msg]
          addr x = Address Nothing (fromString x)
