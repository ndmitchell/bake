
-- | Define a continuous integration system.
module Development.Bake(
    -- * Execute
    bake,
    -- * Central types
    Stringy(..), readShowStringy,
    Oven(..), defaultOven,
    -- ** Oven modifiers
    ovenTest, ovenGit, ovenNotifyStdout, ovenNotifyEmail,
    -- ** TestInfo members
    TestInfo, run, threads, threadsAll, require, suitable,
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
import Development.Bake.Git
import Development.Bake.Email
