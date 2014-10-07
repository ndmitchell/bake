{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Server.Brains(
    brains, Neuron(..)
    ) where

import Development.Bake.Message
import Development.Bake.Type
import Development.Bake.Server.Type
import Data.Maybe
import Control.Monad
import Data.List.Extra


data Neuron
    = Sleep -- nothing useful to do
    | Task Question
    | Update -- update to the active state
    | Reject Patch (Maybe Test) -- reject this patch
    | Broken (Maybe Test) -- the active state with zero patches has ended up broken
      deriving Show

-- Given a ping from a client, figure out what work we can get them to do, if anything
brains :: (Test -> [Test]) -> Server -> Ping -> Neuron
brains _ Server{active=Candidate _ []} _ = Sleep -- no outstanding tasks

brains depends Server{..} Ping{..}
    | allTestsPass active = Update
    | t:_ <- minimumRelation dependsMay $ failingTests active = erroneous t active
    | otherwise = let next = filter (suitableTest active) $ allTests active
                  in taskMay active $ listToMaybe next
    where
        taskMay c t = maybe Sleep (\t -> Task $ Question c t 1 pClient) t
        dependsMay Nothing = []
        dependsMay (Just t) = Nothing : map Just (depends t)

        erroneous t (Candidate s o@(unsnoc -> Just (ps,p))) =
            case (stateTest (Candidate s o) t, stateTest (Candidate s ps) t) of
                (Just True, _) -> error "logical inconsistentcy in brains, expected erroneous test"
                (Just False, Just True) -> Reject p t
                (Just False, Just False) -> erroneous t $ Candidate s ps
                (Nothing, _) -> taskMay (Candidate s o ) $ scheduleTest (Candidate s o ) t
                (_, Nothing) -> taskMay (Candidate s ps) $ scheduleTest (Candidate s ps) t
        erroneous t (Candidate s []) = Broken t

        -- all the tests we know about for this candidate, may be incomplete if Nothing has not passed (yet)
        allTests c = (Nothing:) $ map Just $ concat $ take 1 $
            map (uncurry (++) . aTests . snd) $ success' $ test' Nothing $ answered' $ candidate' c it

        -- are all tests passing for this candidate
        allTestsPass c = flip all (allTests c) $ \t ->
            not $ null $ success' $ test' t $ answered' $ candidate' c it

        -- what tests are failing for this candidate
        failingTests c = map (qTest . fst) $ failure' $ answered' $ candidate' c it

        -- can this candidate start running this test
        suitableTest c t
            | pNowThreads <= 0 = False -- need enough threads
        suitableTest c Nothing
            | null $ self' $ test' Nothing $ candidate' c it -- I am not already running it
            = True
        suitableTest c t@(Just tt)
            | [clientTests] <- map (fst . aTests . snd) $ self' $ success' $ test' Nothing $ answered' $ candidate' c it
            , tt `elem` clientTests -- it is one of the tests this client is suitable for
            , null $ test' t $ self' $ candidate' c it -- I am not running it or have run it
            , clientDone <- map (qTest . fst) $ success' $ answered' $ self' $ candidate' c it
            , all (`elem` clientDone) $ map Just $ depends tt
            = True
        suitableTest _ _ = False

        -- what is the state of this candidate/test, either Just v (aSuccess) or Nothing (not tried)
        stateTest c t = fmap aSuccess $ join $ fmap snd $ listToMaybe $ test' t $ candidate' c it

        -- given that I want to run this particular test, what test should I do next
        -- must pass suitableTest
        scheduleTest c Nothing =
            if suitableTest c Nothing then Just Nothing else Nothing
        scheduleTest c t@(Just tt)
            | [clientTests] <- map (fst . aTests . snd) $ self' $ success' $ test' Nothing $ answered' $ candidate' c it
            , tt `elem` clientTests -- the target is one of the tests this client is suitable for
            = listToMaybe $ filter (suitableTest c) $ transitiveClosure dependsMay t
        scheduleTest c t@(Just tt)
            | null $ self' $ test' Nothing $ candidate' c it -- have never prepared on this client
            = Just Nothing
        scheduleTest _ _ = Nothing

        -- query language
        it = [(q,a) | (_,q,a) <- history]
        candidate' c = filter ((==) c . qCandidate . fst)
        test' t = filter ((==) t . qTest . fst) 
        self' = filter ((==) pClient . qClient . fst) 
        success' = filter (aSuccess . snd)
        failure' = filter (not . aSuccess . snd)
        answered' x = [(q,a) | (q,Just a) <- x]

{-
brains depends Server{..} Ping{..}
    | null failingTests && setupStep == Nothing = (Just $ Question active Nothing 1 pClient, Nothing, False)
    | null failingTests && setupStep == Just Nothing = (Nothing, Nothing, False)
    | null failingTests && null testsTodo = (Nothing, Nothing, True)
    | null failingTests = (fmap (\t -> Question active (Just t) 1 pClient) $ listToMaybe $ filter suitableTest testsTodo, Nothing, False)
    | otherwise = case dropWhile ((== Just False) . snd) $ reverse failingOn of
        [] -> (Nothing, Just $ head patches, False)
        (_,Just True):rest -> (Nothing, Just $ patches !! (length rest + 1), False)
        (ps,Nothing):_ -> error $ "brains, need to attempt with " ++ show ps
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

        Candidate state patches = active

        -- for each patch in the candidate state, does the first failing test fail or not (or unknown)
        failingOn = [ (p,) $ listToMaybe [aSuccess | (_,Question{..},Just Answer{..}) <- history, qCandidate == Candidate state p, qTest == head failingTests]
                    | p <- tail $ inits patches]
-}


transitiveClosure :: Eq a => (a -> [a]) -> a -> [a]
transitiveClosure f = nub . g
    where g x = x : concatMap g (f x)

minimumRelation :: Eq a => (a -> [a]) -> [a] -> [a]
minimumRelation f (x:xs) = [x | disjoint (transitiveClosure f x) xs] ++ minimumRelation f xs
minimumRelation f [] = []
