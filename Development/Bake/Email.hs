
module Development.Bake.Email(ovenEmail) where

import Development.Bake.Type


ovenEmail :: (Host,Port) -> Oven state patch test -> Oven state (patch, [String]) test
ovenEmail = undefined
