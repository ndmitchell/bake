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
    | null failingTests && setupStep == Nothing = (Just $ Question active Nothing 1 pClient, False)
    | null failingTests && setupStep == Just Nothing = (Nothing, False)
    | null failingTests && null testsTodo = (Nothing, True)
    | null failingTests = (fmap (\t -> Question active (Just t) 1 pClient) $ listToMaybe $ filter suitableTest testsTodo, False)
    | otherwise = error "brains, failing tests"
    where
        -- history for those who match the active candidate
        historyActive = filter ((==) active . qCandidate . snd3) history

        -- tests that have failed for the current candidate
        failingTests = [qTest | (_,Question{..},Just Answer{aSuccess=False}) <- historyActive]

        -- Nothing = never run, Just Nothing = in progress, Just (Just t) = completed
        setupStep = listToMaybe [fmap aTests a
            | (_,Question{qTest=Nothing,..},a) <- historyActive, qClient == pClient]

        testsDone = [t | (_,Question{qTest=Just t},Just Answer{aSuccess=True}) <- historyActive]
        testsNeed = let (a,b) = fromJust (fromJust setupStep) in a ++ b
        testsTodo = testsDone \\ testsNeed
        testsDoneMe = [t | (_,Question{qTest=Just t,..},Just Answer{aSuccess=True}) <- historyActive, qClient == pClient]

        -- a test is suitable to run if:
        -- 1) there are enough threads outstanding
        -- 2) this client is capable of running the test
        -- 3) it's dependencies have all been run by this client
        suitableTest t =
            pNowThreads >= 1 &&
            t `elem` fst (fromJust $ fromJust setupStep) &&
            all (`elem` testsDoneMe) (depends t)
