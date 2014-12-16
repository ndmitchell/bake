
module General.MRU(
    MRU, empty, insert, lookup, delete, toList
    ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Tuple.Extra


-- Basically a bimap where we can do minview on the second element
data MRU k v = MRU
    Int -- usage index
    (Map.Map k (Int, v)) -- when did I last use this thing
    (IntMap.IntMap k) -- what was used at which point


empty :: MRU k v
empty = MRU 0 Map.empty IntMap.empty

toList :: MRU k v -> [(k, v)]
toList (MRU _ mp1 _) = map (second snd) $ Map.toList mp1

insert :: Ord k => k -> v -> MRU k v -> MRU k v
insert k v (MRU n mp1 mp2) = MRU (n+1) (Map.insert k (n, v) mp1) (IntMap.insert n k mp22)
    where mp22 = case Map.lookup k mp1 of Nothing -> mp2; Just (i,_) -> IntMap.delete i mp2

lookup :: Ord k => k -> MRU k v -> Maybe v
lookup k (MRU _ mp1 _) = fmap snd $ Map.lookup k mp1

delete :: Ord k => MRU k v -> Maybe (k, MRU k v)
delete (MRU n mp1 mp2) = case IntMap.minViewWithKey mp2 of
    Nothing -> Nothing
    Just ((i,k), mp2) -> Just (k, MRU n (Map.delete k mp1) mp2)
