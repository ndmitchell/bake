
module Development.Bake.Email(ovenNotifyEmail) where

import Development.Bake.Type


-- | Email notifications when users should be notified about success/failure.
--   Requires the host/port of an SMTP server. Not yet implemented, will always crash.
ovenNotifyEmail :: (Host,Port) -> Oven state patch test -> Oven state patch test
ovenNotifyEmail hp o = o{ovenNotify = \a s -> sendEmail hp a s >> ovenNotify o a s}

sendEmail :: (Host,Port) -> [Author] -> String -> IO ()
sendEmail = error "Bake.sendEmail not yet implemented"
