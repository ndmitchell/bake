
module Development.Bake.Email(ovenNotifyEmail) where

import Development.Bake.Core.Type
import Network.Mail.SMTP
import Data.String
import Data.List.Extra


-- | Email notifications when users should be notified about success/failure.
ovenNotifyEmail :: (Host,Port) -> (Author -> [String]) -> Oven state patch test -> Oven state patch test
ovenNotifyEmail hp resolve = ovenNotifyAdd $ \a s -> sendEmail hp (nubOrd $ concatMap resolve a) s

sendEmail :: (Host,Port) -> [String] -> String -> IO ()
sendEmail (h,p) to msg = sendMail' h (fromIntegral p) email
    where email = simpleMail (addr "bake@example.com") (map addr to) [] [] (fromString "Bake notification") [plainTextPart $ fromString msg]
          addr x = Address Nothing (fromString x)
