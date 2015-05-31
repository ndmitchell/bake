{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Bake.Server.Property(
    rejectable, plausible, mergeable,
    extendActive, restrictActive
    ) where

import Development.Bake.Server.Memory
import Development.Bake.Server.Store
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Bake.Core.Type
import Data.Maybe
import General.Extra
import Data.List


-- | I can reject the tests rejected because of the given patches
rejectable :: Memory -> [(Patch, Maybe Test)]
-- only look at failing tests in the current state
-- find tests which have a passed/failed one apart
-- assume the state passes everything
-- if a test isn't avaiable at a point, it passes
rejectable Memory{..} =
    [(last ps, t)
        | ps <- tail $ inits $ snd active
        , let me = storePoint store (fst active, ps)
        , let prev = if length ps == 1 then piState else storePoint store (fst active, init ps)
        , t <- failed
        , poTest me t == Just False && poTest prev t == Just True]
    where
        piState = PointInfo (Just Set.empty) (Set.singleton Nothing) Set.empty

        -- tests that are failing in self, interesting to consider
        failed = Set.toList $ poFail $ storePoint store active


-- | I can mark all active patches as plausible
plausible :: Memory -> Bool
plausible Memory{..}
    | all (isNothing . paReject . storePatch store) $ snd active
    , PointInfo{..} <- storePoint store active
    , Just tests <- poTodo
    , Set.null poFail
    , tests == Set.union (storeSupersetPass store active) (catMaybesSet poPass)
    = True
plausible _ = False


-- | I can merge all active patches
mergeable :: Memory -> Bool
mergeable mem@Memory{..}
    | plausible mem
    , PointInfo{..} <- storePoint store active
    , Just tests <- poTodo
    , Set.size tests + 1 == Set.size poPass
    = True
mergeable _ = False


-- | Add in all extra patches that are queued
extendActive :: Memory -> Bool
-- either there are no patches being tested, or the ones being tested are all plausible
-- relies on throwing out the rejected ones with restrictActive first
extendActive Memory{..} = all (isJust . paPlausible . storePatch store) $ snd active


-- | Throw out the patches that have been rejected
restrictActive :: Oven State Patch Test -> Memory -> Bool
restrictActive oven Memory{..}
    -- I can reject someone for failing preparation
    | Nothing `Set.member` rejectedTests = True

    -- if all tests either (passed on active or a superset)
    --              or     (failed on active and lead to a rejection)
    --              or     (depend on a test that failed)
    | not $ Set.null rejectedTests
    , PointInfo{..} <- storePoint store active
    , Just tests <- poTodo
    , let pass = Set.union (storeSupersetPass store active) $ catMaybesSet $ poPass `Set.difference` poFail
    , flip all (Set.toList tests) $ \t ->
        t `Set.member` pass || any (flip Set.member rejectedTests . Just) (transitiveClosure (testDepend . ovenTestInfo oven) [t]) = True

    | otherwise = False

    where
        rejectedTests = Set.unions $ mapMaybe (fmap (Map.keysSet . snd) . paReject . storePatch store) $ snd active
