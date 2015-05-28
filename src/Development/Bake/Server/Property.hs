{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Server.Property(
    rejectable, plausible, mergeable,
    extendActive, restrictActive
    ) where

import Data.IORef
import Development.Bake.Server.Disk
import qualified Data.Map as Map
import qualified Data.Set as Set
import Development.Bake.Core.Type
import Data.Time
import Control.Monad.Extra
import Data.Maybe
import Data.Tuple.Extra
import Development.Bake.Core.Message
import System.IO.Unsafe
import General.Extra
import Development.Bake.Server.Memory
import Data.List


-- | I can reject the tests rejected because of the given patches
rejectable :: Memory -> Cache -> [(Patch, Maybe Test)]
-- only look at failing tests in the current state
-- find tests which have a passed/failed one apart
-- assume the state passes everything
-- if a test isn't avaiable at a point, it passes
rejectable Memory{..} Cache{..} =
        [(p, t) | (p,me,prev) <- zip3 patches points (tail points), t <- failed
                , pass t me == Just False && pass t prev == Just True]
    where
        -- tests that are failing in self, interesting to consider
        failed = [t | (t, xs) <- Map.toList $ pointRuns selfInfo, any (not . fst) xs]

        patches = reverse $ snd active
        points = selfInfo : reverse previousInfo ++
            [PointEx (Just Set.empty) (Map.singleton Nothing [(True, Client "")]) Set.empty True] -- fake state point

        pass t PointEx{..}
            | xs@(_:_) <- Map.findWithDefault [] t pointRuns = Just $ all fst xs -- I have a result for it
            | Just t <- t, Just todo <- pointTests, not $ t `Set.member` todo = Just True -- It's not applicable, and thus passes
            | otherwise = Nothing


-- | I can mark all active patches as plausible
plausible :: Memory -> Cache -> Bool
plausible Memory{..} Cache{..}
    | all (Set.null . pointRejected) $ selfInfo : previousInfo
    , Just tests <- pointTests selfInfo
    , all (all fst) $ Map.elems $ pointRuns selfInfo
    , tests == Set.union supersetPass (catMaybesSet $ Map.keysSet $ pointRuns selfInfo)
    = True
plausible _ _ = False


-- | I can merge all active patches
mergeable :: Memory -> Cache -> Bool
mergeable mem@Memory{..} cache@Cache{..}
    | plausible mem cache
    , Just tests <- pointTests selfInfo
    , Set.size tests + 1 == Map.size (pointRuns selfInfo)
    = True
mergeable _ _ = False


-- | Add in all extra patches that are queued
extendActive :: Memory -> Cache -> Bool
-- either there are no patches being tested, or the ones being tested are all plausible
-- relies on throwing out the rejected ones with restrictActive first
extendActive Memory{..} Cache{..} = null (snd active) || all pointPlausible (selfInfo : previousInfo)


-- | Throw out the patches that have been rejected
restrictActive :: Oven State Patch Test -> Memory -> Cache -> Bool
restrictActive oven Memory{..} Cache{..}
    -- I can reject someone for failing preparation
    | Nothing `Set.member` rejectedTests = True

    -- if all tests either (passed on active or a superset)
    --              or     (failed on active and lead to a rejection)
    --              or     (depend on a test that failed)
    | not $ Set.null rejectedTests
    , Just tests <- pointTests selfInfo
    , flip all (Set.toList tests) $ \t ->
        t `Set.member` passedTests || any (flip Set.member rejectedTests . Just) (transitiveClosure (testDepend . ovenTestInfo oven) [t]) = True

    | otherwise = False

    where
        rejectedTests = Set.unions $ map pointRejected $ selfInfo : previousInfo
        passedTests = Set.union supersetPass $ catMaybesSet $ Map.keysSet $ Map.filter (all fst) $ pointRuns selfInfo
