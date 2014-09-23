
-- | Define a continuous integration system.
module Development.Bake(
    -- * Execute
    bake,
    -- * Central types
    Candidate(..), Oven(..), TestInfo,
    defaultOven, run,
    -- ** TestInfo mutators
    threads, threadsAll, before, beforeClear, require,
    -- * Operations
    startServer, startClient,
    module Development.Bake.Send,
    -- * Utility types
    Host, Port
    ) where

import Development.Bake.Type
import Development.Bake.Server.Start
import Development.Bake.Client
import Development.Bake.Args
import Development.Bake.Send
