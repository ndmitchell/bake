{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Query(
    asked, test', candidateExact', answered, success', client'
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import Development.Bake.Server.Type


type Query = Server -> Question -> Maybe Answer -> Bool

(&&^) :: Query -> Query -> Query
(&&^) f g s q a = f s q a && g s q a

true' :: Query
true' _ _ _ = True


asked :: Server -> [Query] -> [(Question, Maybe Answer)]
asked server qs = [(q, a) | (_, q, a) <- history server, pred q a]
    where pred = foldl (&&^) true' qs server

answered :: Server -> [Query] -> [(Question, Answer)]
answered server query = [(q, a) | (q, Just a) <- asked server query]

success' :: Query
success' _ _ = maybe False aSuccess

client' :: Client -> Query
client' c _ q _ = qClient q == c

test' :: Maybe Test -> Query
test' t _ q _ = qTest q == t

candidateExact' :: (State, [Patch]) -> Query
candidateExact' c _ q _ = qCandidate q == c
