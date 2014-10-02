{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Server.Brains(
    brains
    ) where

import Development.Bake.Message
import Development.Bake.Server.Type
import Data.Time.Clock


-- Given a ping from a client, figure out what work we can get them to do, if anything
-- Any client who hasn't ping'd us since cutoff is considered dead
brains :: UTCTime -> Server -> Ping -> (Maybe Question, Bool)
brains cutoff Server{..} Ping{..} = error "brains"
    where
        -- clients who haven't responded since cutoff
        -- deadClients = undefined

        -- tests that have failed for the current candidate
        -- failingTest = undefined
