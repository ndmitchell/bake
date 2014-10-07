
module Development.Bake.Email(ovenNotifyEmail) where

import Development.Bake.Type


ovenNotifyEmail :: (Host,Port) -> Oven state patch test -> Oven state patch test
ovenNotifyEmail hp o = o{ovenNotify = \a s -> sendEmail hp a s >> ovenNotify o a s}

sendEmail :: (Host,Port) -> [Author] -> String -> IO ()
sendEmail = error "Bake.sendEmail not yet implemented"
