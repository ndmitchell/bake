{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Server.Brains(
    brains, Neuron(..)
    ) where

import Development.Bake.Core.Message
import Development.Bake.Core.Type
import Development.Bake.Server.Type
import Development.Bake.Server.Algebra
import Data.Maybe
import Control.Monad
import Data.List.Extra


data Neuron
    = Sleep -- nothing useful to do
    | Task Question
    | Update (State, [Patch])-- update to the target state
    | Reject Patch (Maybe Test) -- reject this patch
    | Broken (Maybe Test) -- the target state with zero patches has ended up broken
      deriving Show

-- Given a ping from a client, figure out what work we can get them to do, if anything
brains :: (Test -> TestInfo Test) -> Server -> Ping -> Neuron
brains info server@Server{..} Ping{..}
    | bless@(_:_) <- targetBlessedPrefix server = Update (fst target, bless)
    | blessedState server target = Sleep
    | t:_ <- minimumRelation dependsMay $ failingTests target = erroneous t target
    | otherwise = let next = filter (suitableTest target) $ allTests target
                  in taskMay target $ listToMaybe next
    where
        taskMay c t = maybe Sleep (\t -> Task $ Question c t (threadsForTest t) pClient) t
        dependsMay Nothing = []
        dependsMay (Just t) = Nothing : map Just (testRequire $ info t)

        erroneous t (s, o@(unsnoc -> Just (ps,p))) =
            case (stateTest (s, o) t, stateTest (s, ps) t) of
                (Just True, _) -> error "logical inconsistentcy in brains, expected erroneous test"
                (Just False, Just True) -> Reject p t
                (Just False, Just False) -> erroneous t (s,ps)
                (Nothing, _) -> taskMay (s, o ) $ scheduleTest (s, o ) t
                (_, Nothing) -> taskMay (s, ps) $ scheduleTest (s, ps) t
        erroneous t (s, []) = Broken t

        -- all the tests we know about for this candidate, may be incomplete if Nothing has not passed (yet)
        allTests c = (Nothing:) $ map Just $ concat $ take 1 $
            map (aTests . snd) $ success_ $ test_ Nothing $ answered_ $ candidate_ c it

        -- what tests are failing for this candidate
        failingTests c = map (qTest . fst) $ failure_ $ answered_ $ candidate_ c it

        -- how many threads does this test require
        threadsForTest = maybe 1 (fromMaybe pMaxThreads . testThreads . info)

        -- can this candidate start running this test
        suitableTest c t
            | threadsForTest t > pNowThreads = False -- not enough threads
        suitableTest c Nothing
            | null $ self_ $ test_ Nothing $ candidate_ c it -- I am not already running it
            = True
        suitableTest c t@(Just tt)
            | [clientTests] <- map (fst . aTestsSuitable . snd) $ self_ $ success_ $ test_ Nothing $ answered_ $ candidate_ c it
            , tt `elem` clientTests -- it is one of the tests this client is suitable for
            , null $ test_ t $ self_ $ candidate_ c it -- I am not running it or have run it
            , clientDone <- map (qTest . fst) $ success_ $ answered_ $ self_ $ candidate_ c it
            , all (`elem` clientDone) $ map Just $ testRequire $ info tt
            = True
        suitableTest _ _ = False

        -- what is the state of this candidate/test, either Just v (aSuccess) or Nothing (not tried)
        stateTest c t = fmap aSuccess $ join $ fmap snd $ listToMaybe $ test_ t $ candidate_ c it

        -- given that I want to run this particular test, what test should I do next
        -- must pass suitableTest
        scheduleTest c Nothing =
            if suitableTest c Nothing then Just Nothing else Nothing
        scheduleTest c t@(Just tt)
            | [clientTests] <- map (fst . aTestsSuitable . snd) $ self_ $ success_ $ test_ Nothing $ answered_ $ candidate_ c it
            , tt `elem` clientTests -- the target is one of the tests this client is suitable for
            = listToMaybe $ filter (suitableTest c) $ transitiveClosure dependsMay t
        scheduleTest c t@(Just tt)
            | null $ self_ $ test_ Nothing $ candidate_ c it -- have never prepared on this client
            = Just Nothing
        scheduleTest _ _ = Nothing

        -- query language
        it = [(q,a) | (_,q,a) <- history]
        candidate_ c = filter ((==) c . qCandidate . fst)
        test_ t = filter ((==) t . qTest . fst) 
        self_ = filter ((==) pClient . qClient . fst) 
        success_ = filter (aSuccess . snd)
        failure_ = filter (not . aSuccess . snd)
        answered_ x = [(q,a) | (q,Just a) <- x]


transitiveClosure :: Eq a => (a -> [a]) -> a -> [a]
transitiveClosure f = nub . g
    where g x = x : concatMap g (f x)

minimumRelation :: Eq a => (a -> [a]) -> [a] -> [a]
minimumRelation f (x:xs) = [x | disjoint (transitiveClosure f x) xs] ++ minimumRelation f xs
minimumRelation f [] = []
