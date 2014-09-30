
module Development.Bake.Server.Brains(
    brains
    ) where

import Development.Bake.Message
import Development.Bake.Server.Type


-- Given a ping from a client, figure out what work we can get them to do, if any
brains :: Server -> Ping -> [Question]
brains = error "brains"
