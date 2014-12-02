{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Query(
    Query, true', false', (&&^), (||^),
    asked, answered,
    translate',
    unanswered', success', failure', test',
    candidate', candidateBy', patch', blame', lastPatch'
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import Development.Bake.Server.Type
import Data.Maybe
import Data.Tuple.Extra
import Data.List.Extra


type Query = Server -> Question -> Maybe Answer -> Bool

(&&^), (||^) :: Query -> Query -> Query
(&&^) f g s q a = f s q a && g s q a
(||^) f g s q a = f s q a || g s q a

false', true' :: Query
false' _ _ _ = False
true' _ _ _ = True


asked :: Server -> [Query] -> [(Question, Maybe Answer)]
asked server qs = [(q, a) | (_, q, a) <- history server, pred q a]
    where pred = foldl (&&^) true' qs server

answered :: Server -> [Query] -> [(Question, Answer)]
answered server query = [(q, a) | (q, Just a) <- asked server query]

unanswered' :: Query
unanswered' _ _ a = isNothing a

success', failure' :: Query
success' _ _ = maybe False aSuccess
failure' _ _ = maybe False (not . aSuccess)


test' :: Maybe Test -> Query
test' t _ q _ = qTest q == t

candidate' :: (State, [Patch]) -> Query
candidate' (s,ps) = candidateBy' s (== ps)

candidateBy' :: State -> ([Patch] -> Bool) -> Query
candidateBy' s p server q a = maybe False p $ translate server s (qCandidate q)


patch' :: Patch -> Query
patch' p server Question{qCandidate=(s,ps)} a = p `elem` ps ++ concatMap (snd . thd3) upds
    where upds = dropWhile ((/=) s . snd3) $ updates server

lastPatch' :: Patch -> Query
lastPatch' p server q a = maybe False ((==) p . snd) $ unsnocPatch server $ qCandidate q

translate' :: Server -> State ->  [(Question, a)] ->  [(Question, a)]
translate' server s xs = [(q{qCandidate=(s,p)}, a) | (q,a) <- xs, Just p <- [translate server s $ qCandidate q]]


-- | Returns True if a question/answer pair is responsible for attaching blame.
--   That requires this to have passed, but a previous step to have failed
blame' :: Query
blame' server Question{..} (Just Answer{..})
    | not aSuccess
    , Just (c, _) <- unsnocPatch server qCandidate
    = not $ null $ answered server [candidate' c, success', test' qTest]
    where
blame' _ _ _ = False


unsnocPatch :: Server -> (State, [Patch]) -> Maybe ((State, [Patch]), Patch)
unsnocPatch server (s, ps)
    | Just (ps, p) <- unsnoc ps = Just ((s, ps), p)
    | Just (_, _, r) <- find ((==) s . snd3) $ updates server = unsnocPatch server r
    | otherwise = Nothing
