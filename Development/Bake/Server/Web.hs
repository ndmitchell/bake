
-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Web

web :: Payload -> Server -> IO (Either FilePath String)
web = undefined
