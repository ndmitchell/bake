{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Server.Brains(
    brains, Neuron(..)
    ) where

import Development.Bake.Core.Message
import Development.Bake.Core.Type
import Development.Bake.Server.Type
import Development.Bake.Server.Query
import Control.Applicative
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra


data Neuron
    = Sleep -- nothing useful to do
    | Task Question
    | Update (State, [Patch])-- update to the target state
    | Reject Patch (Maybe Test) -- reject this patch
    | Broken (Maybe Test) -- the target state with zero patches has ended up broken
      deriving (Show,Eq)

-- Given a ping from a client, figure out what work we can get them to do, if anything
brains :: (Test -> TestInfo Test) -> Server -> Ping -> Neuron
brains info server@Server{..} Ping{..}
    | bless@(_:_) <- targetBlessedPrefix server = Update (fst target, bless)
    | blessedState server target = Sleep
    | blame:_ <- targetBlame server = uncurry Reject blame
    | t:_ <- failures, null (snd target) = Broken $ fst t
    | (c,t):_ <- filter (uncurry suitableTest) $ unasked todoFail ++ unasked todoPass
        = Task $ Question c t (threadsForTest t) pClient
    | otherwise = Sleep
    where
        failures = targetFailures server

        unasked xs = no ++ yes
            where started = map (qCandidate &&& qTest) $ map fst $ asked server []
                  (yes, no) = partition (`elem` started) xs

        -- all the tests, sorted so those which have been done least are first
        todoPass = map ((target,) . fst) $ sortOn (negate . length . nub . concat . snd) $ groupSort $
            [(qTest, snd qCandidate) | (Question{..},_) <- translate' server (fst target) $ answered server [success']] ++
            map (,[]) (allTests target)

        todoFail = [((fst target, init ps), t) | (t, ps@(_:_)) <- failures, t <- dependencies t]


        dependencies = transitiveClosure $ \t -> case t of
            Nothing -> []
            Just t -> Nothing : map Just (testRequire $ info t)

        -- all the tests we know about for this candidate, may be incomplete if Nothing has not passed (yet)
        allTests c = (Nothing:) $ map Just $ concat $ take 1 $
            map (aTests . snd) $ success_ $ test_ Nothing $ answered_ $ candidate_ c it

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
            , all (`elem` clientDone) $ map Just $ testRequire $ info tt -- I have done all the dependencies
            = True
        suitableTest _ _ = False


        -- query language
        it = [(q,a) | (_,q,a) <- history]
        candidate_ c = filter ((==) c . qCandidate . fst)
        test_ t = filter ((==) t . qTest . fst) 
        self_ = filter ((==) pClient . qClient . fst) 
        success_ = filter (aSuccess . snd)
        answered_ x = [(q,a) | (q,Just a) <- x]


transitiveClosure :: Eq a => (a -> [a]) -> a -> [a]
transitiveClosure f = nub . g
    where g x = x : concatMap g (f x)


-- | Given the current target, what prefix is already blessed.
--   Usually the empty list, can immediately be rolled into the target.
targetBlessedPrefix :: Server -> [Patch]
targetBlessedPrefix server@Server{..} = head $ filter isBlessed $ reverse $ inits $ snd target
    where
        isBlessed [] = True
        isBlessed ps = blessedState server (fst target, ps)


-- | Given a State, is it blessed
blessedState :: Server -> (State, [Patch]) -> Bool
blessedState server c
    | let f t = answered server [test' t, success', candidate' c]
    , todo:_ <- aTests . snd <$> f Nothing
    = all (not . null . f . Just) todo
blessedState _ _ = False


-- | Which failures have occured for patches whose prefix is in the target.
--   The earliest failure (by timestamp) will be first
targetBlame :: Server -> [(Patch, Maybe Test)]
targetBlame server@Server{..} =
    [ (last $ snd $ qCandidate q, qTest q)
    | (q, a) <- translate' server (fst target) $ answered server
        [blame', candidateBy' (fst target) $ \ps -> ps `isPrefixOf` snd target && not (null ps)]]
