{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Email(ovenNotifyEmail) where

import Development.Bake.Core.Type
import Network.Mail.SMTP
import Data.String


-- | Send notifications using the given SMTP host/port.
ovenNotifyEmail :: (Host, Port) -> Oven state patch test -> Oven state patch test
ovenNotifyEmail (h,p) = ovenNotifyAdd $ \author subject body -> do
    sendMail' h (fromIntegral p) $ simpleMail
        (addr "bake@example.com") [addr author] [] []
        (fromString $ "[Bake] " ++ subject) [htmlPart $ fromString body]
    where addr x = Address Nothing (fromString x)
