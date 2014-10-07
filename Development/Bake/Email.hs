
module Development.Bake.Email(ovenEmail) where

import Development.Bake.Type


ovenEmail :: (Host,Port) -> Oven state patch test -> Oven state patch test
ovenEmail hp o = o{ovenNotify = \a s -> sendEmail hp a s >> ovenNotify o a s}

sendEmail :: (Host,Port) -> [Author] -> String -> IO ()
sendEmail = error "sendEmail"
