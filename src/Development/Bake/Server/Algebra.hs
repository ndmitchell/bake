{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Algebra(
    blessedTargetPrefix, blessedState,
    targetFailures,
    Algebra(..), algebraZero, algebraPatch
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import Development.Bake.Server.Type
import Development.Bake.Server.Query
import Data.Tuple.Extra
import Data.Maybe
import Control.Applicative
import Data.List.Extra


-- | Given the current target, what prefix is already blessed.
--   Usually the empty list, can immediately be rolled into the target.
blessedTargetPrefix :: Server -> [Patch]
blessedTargetPrefix server@Server{..} = head $ filter isBlessed $ reverse $ inits $ snd target
    where
        isBlessed [] = True
        isBlessed ps = blessedState server (fst target, ps)


blessedState :: Server -> (State, [Patch]) -> Bool
blessedState server c
    | let f t = answered server [test' t, success', candidate' c]
    , todo:_ <- aTests . snd <$> f Nothing
    = all (not . null . f . Just) todo
blessedState _ _ = False




-- | Which failures have occured for patches whose prefix is in the target.
--   The earliest failure (by timestamp) will be first
targetFailures :: Server -> [(Maybe Test, [Patch])]
targetFailures server@Server{..} =
    [ (qTest q, snd $ qCandidate q)
    | (q, a) <- translate' server (fst target) $ answered server
        [failure', candidateBy' (fst target) (`isPrefixOf` snd target)]]


data Algebra
    = Unknown                    -- ^ Been given, not yet been prepared
    | Progressing [Test] [Test]  -- ^ In progress, on left have been done, on right still todo.
                                 --   Once all done, not yet accepted, merely plausible.
    | Accepted                   -- ^ Accepted, rolled into production
    | Paused                     -- ^ In the pause queue
    | Rejected [Question]        -- ^ Rejected, because of the following tests

-- Note, when deciding rejected look for the last result to say failure
-- since blacklist may have meant earlier ones weren't relevant failures

-- | Algebra for the initial state.
algebraZero :: Server -> Algebra
algebraZero server
    | not $ null $ updates server = Accepted
algebraZero server
    | fails@(_:_) <- answered server [candidate' (state0 server, []), failure']
    = Rejected $ map fst fails
algebraZero server
    | total:_ <- map (aTests . snd) $ answered server [test' Nothing, candidate' (state0 server, [])]
    , done <- nub $ mapMaybe (qTest . fst) $ answered server [candidate' (state0 server, [])]
    , todo <- total \\ done
    = if null todo then Accepted else Progressing done todo
algebraZero _ = Unknown


-- | Nothing stands for using the zero patch on the initial state 
algebraPatch :: Server -> Patch -> Algebra
algebraPatch server p
    | p `elem` concatMap (snd . thd3) (updates server) = Accepted
    | p `elem` maybe [] (map snd) (paused server) = Paused
algebraPatch server p
    -- we may have previously failed, but been requeued, so if we're active don't hunt for reject
    | p `notElem` snd (target server)
    , poss <- [ ((second init qCandidate, qTest), q)
              | (_,q@Question{..},Just Answer{..}) <- history server
              , not aSuccess, [p] `isSuffixOf` snd qCandidate]
    , real <- [ q
              | (_,Question{..},Just Answer{..}) <- history server
              , aSuccess
              , Just q <- [lookup (qCandidate, qTest) poss]]
    , not $ null real
    = Rejected $ nub real
algebraPatch server p
    | total:_ <- map (aTests . snd) $ answered server [test' Nothing, patch' p]
    , done <- nub $ mapMaybe (qTest . fst) $ answered server [patch' p, success']
    , todo <- total \\ done
    = Progressing done todo
algebraPatch _ _ = Unknown
