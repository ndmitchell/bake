
-- | A continuous integration system. For an example of how to get started
--   see <https://github.com/ndmitchell/bake#readme>.
module Development.Bake(
    -- * Execute
    bake,
    -- * Central types
    Oven(..), defaultOven,
    Stringy(..), readShowStringy,
    -- ** Oven modifiers
    ovenTest, ovenGit, ovenNotifyStdout, ovenNotifyEmail,
    -- ** TestInfo members
    TestInfo, run, threads, threadsAll, require, suitable,
    -- * Operations
    startServer, startClient,
    module Development.Bake.Send,
    -- * Utility types
    Host, Port, Author
    ) where

import Development.Bake.Type
import Development.Bake.Server.Start
import Development.Bake.Client
import Development.Bake.Args
import Development.Bake.Send
import Development.Bake.Git
import Development.Bake.Email
