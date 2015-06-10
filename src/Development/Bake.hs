
-- | A continuous integration system. For an example of how to get started
--   see <https://github.com/ndmitchell/bake#readme>.
module Development.Bake(
    -- * Execute
    bake,
    -- * Central types
    Oven(..), defaultOven,
    Stringy(..),
    -- ** Oven modifiers
    ovenTest,
    SHA1, ovenGit, ovenStepGit,
    ovenNotifyAdd, ovenNotifyStdout, ovenNotifyEmail,
    ovenPretty, ovenPrettyMerge,
    -- ** TestInfo members
    TestInfo, run, threads, threadsAll, depend, require, priority,
    -- * Operations
    startServer, startClient, garbageCollect,
    module Development.Bake.Core.Send,
    -- * Utility types
    Host, Port, Author
    ) where

import Development.Bake.Core.Type
import Development.Bake.Server.Start
import Development.Bake.Core.Client
import Development.Bake.Core.Args
import Development.Bake.Core.GC
import Development.Bake.Core.Send
import Development.Bake.Git
import Development.Bake.StepGit
import Development.Bake.Pretty
import Development.Bake.Email
