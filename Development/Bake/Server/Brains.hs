{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Server.Brains(
    brains
    ) where

import Development.Bake.Message
import Development.Bake.Type
import Development.Bake.Server.Type
import Development.Bake.Util
import Data.List
import Data.Maybe


-- Given a ping from a client, figure out what work we can get them to do, if anything
-- Any client who hasn't ping'd us since cutoff is considered dead
brains :: (Test -> [Test]) -> Server -> Ping -> (Maybe Question, Bool {- should I update -})
brains _ Server{active=Candidate _ []} _ = (Nothing, False) -- no outstanding tasks
brains depends Server{..} Ping{..}
    | null failingTests && null todoTests = (Nothing, True)
    | null failingTests = (fmap (\t -> Question active t 1 pClient) $ listToMaybe $ filter suitableTest todoTests, False)
    | otherwise = error "brains, failing tests"
    where
        -- history for those who match the active candidate
        historyActive = filter ((==) active . qCandidate . snd3) history

        -- tests that have failed for the current candidate
        failingTests = [qTest | (_,Question{..},Just Answer{aStatus=Failure}) <- historyActive]

        -- tests that have not been done on the current candidate, but were asked for
        todoTests =
            (Nothing : [Just t | (_,_,Just Answer{..}) <- historyActive, t <- aTests]) \\
            [qTest | (_,Question{..},Just Answer{aStatus=Success}) <- historyActive]

        -- a test is suitable to run if:
        -- 1) this client has never replied NotApplicable
        -- 2) it's dependencies have all been run by this client
        -- 3) there are enough threads outstanding
        suitableTest t =
            pNowThreads >= 1 &&
            null [() | (_,Question{..},Just Answer{aStatus=NotApplicable}) <- historyActive, qTest == t] &&
            null (maybe [] (map Just . depends) t `intersect` todoTests)
