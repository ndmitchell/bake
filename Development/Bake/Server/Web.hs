
-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Web

web :: Input -> Server -> IO Output
web _ s = return $ OutputString $ show s
