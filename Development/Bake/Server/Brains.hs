{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Server.Brains(
    brains
    ) where

import Development.Bake.Message
import Development.Bake.Type
import Development.Bake.Server.Type
import Development.Bake.Util
import Data.List


-- Given a ping from a client, figure out what work we can get them to do, if anything
-- Any client who hasn't ping'd us since cutoff is considered dead
brains :: Server -> Ping -> (Maybe Question, Maybe (Candidate State Patch))
brains Server{active=Candidate _ []} _ = (Nothing, Nothing) -- no outstanding tasks
brains Server{..} Ping{..}
    | null failingTests && null todoTests = (Nothing, Just active)
    | null failingTests = undefined
    | otherwise = undefined
    where
        -- history for those who match the active candidate
        historyActive = filter ((==) active . qCandidate . snd3) history

        -- tests that have failed for the current candidate
        failingTests = [qTest | (_,Question{..},Just Answer{aStatus=Failure}) <- historyActive]

        -- tests that have not been done on the current candidate, but were asked for
        todoTests =
            (Nothing : [Just t | (_,_,Just Answer{..}) <- historyActive, t <- aTests]) \\
            [qTest | (_,Question{..},Just Answer{aStatus=Success}) <- historyActive]
