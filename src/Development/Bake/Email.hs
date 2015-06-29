
module Development.Bake.Email(ovenNotifyEmail) where

import Development.Bake.Core.Type
import Network.Mail.SMTP
import Data.String
import Data.List.Extra


-- | Email notifications when users should be notified about success/failure.
ovenNotifyEmail :: (Host,Port) -> (Author -> [String]) -> Oven state patch test -> Oven state patch test
ovenNotifyEmail hp resolve = ovenNotifyAdd $ \subject message ->
    sendEmail hp ("[Bake] " ++ subject) (nubOrd $ concatMap (resolve . fst) message) $ unlines $ map snd message


sendEmail :: (Host,Port) -> String -> [String] -> String -> IO ()
sendEmail (h,p) subject to msg = sendMail' h (fromIntegral p) email
    where email = simpleMail (addr "bake@example.com") (map addr to) [] [] (fromString subject) [plainTextPart $ fromString msg]
          addr x = Address Nothing (fromString x)
