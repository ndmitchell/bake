{-# LANGUAGE RecordWildCards #-}

-- Stuff on disk on the server
module Development.Bake.Server.Store(
    Store, newStore,
    PointInfo(..), poTest, PatchInfo(..),
    storePatches, storePatch, storeAlive, storePoint, storeSupersetPass,
    Update(..), storeUpdate
    ) where

import Control.Exception.Extra
import Control.Concurrent.Extra
import Development.Bake.Core.Type
import Data.IORef
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import System.Time.Extra
import System.FilePath
import Control.Monad.Extra
import System.Directory.Extra
import Data.Tuple.Extra
import Data.Unique
import Data.Hashable
import qualified Data.Bimap as Bimap
import qualified General.MRU as MRU


data PointInfo = PointInfo
    {poTodo :: Maybe (Set.Set Test)
    ,poPass :: Set.Set (Maybe Test)
    ,poFail :: Set.Set (Maybe Test) -- may be in both pass and fail
    }

poTest :: PointInfo -> Maybe Test -> Maybe Bool
poTest PointInfo{..} t
    | t `Set.member` poFail = Just False
    | t `Set.member` poPass = Just True
    | Just t <- t, Just todo <- poTodo, not $ t `Set.member` todo = Just True -- It's not applicable, and thus passes
    | otherwise = Nothing


data PatchInfo = PatchInfo
    {paQueued :: (UTCTime, Author)
    ,paStart :: Maybe UTCTime
    ,paDelete :: Maybe UTCTime
    ,paReject :: Maybe (UTCTime, Map.Map (Maybe Test) Point)
    ,paPlausible :: Maybe UTCTime
    ,paMerge :: Maybe UTCTime
    }

data Store = Store
    {time :: UTCTime
    ,pointMap :: Map.Map PointHash Point -- ^ Points I have any information about (so I can store only hashes, and enumerate points) 
    ,stateMap :: Map.Map State Point -- ^ States produced from patches
    ,patchInfo :: Map.Map Patch PatchInfo -- ^ Information about all patches
    ,patchAlive :: Set.Set Patch -- ^ Patches that are alive
    ,points :: MRU.MRU Point PointInfo -- needs to be in IO for cache properties
    ,supersets :: Maybe (Point, Set.Set Test) -- needs to be in IO for cache properties
    }

newStore :: FilePath -> IO Store
newStore = undefined

storePoint :: Store -> (State,[Patch]) -> PointInfo
storePoint = undefined

storePatches :: Store -> Map.Map Patch PatchInfo
storePatches = undefined

storePatch :: Store -> Patch -> PatchInfo
storePatch = undefined

storeAlive :: Store -> Set.Set Patch
storeAlive = undefined

storeSupersetPass :: Store -> (State,[Patch]) -> Set.Set Test
storeSupersetPass = undefined


data Update
    = IUState State (State, [Patch])
    | IUQueue Patch Author
    | IUStart Patch
    | IUDelete Patch
    | IUReject Patch (Maybe Test) (State, [Patch])
    | IUPlausible Patch
    | IUSupersede Patch
    | IUMerge Patch
    | PUTest (State, [Patch]) [Test]
    | PUPass (State, [Patch]) (Maybe Test)
    | PUFail (State, [Patch]) (Maybe Test)


storeUpdate :: Store -> [Update] -> Store
storeUpdate = undefined


data Point = Point (State, [Patch]) PointHash deriving Show

newtype PointHash = PointHash Int deriving (Eq,Ord,Show)

instance Eq Point where
    Point _ a == Point _ b = a == b

instance Ord Point where
    compare (Point _ a) (Point _ b) = compare a b

newPoint :: (State, [Patch]) -> Point
newPoint (s, ps) = Point (s,ps) (PointHash $ hash (fromState s, map fromPatch ps))

fromPoint :: Point -> (State, [Patch])
fromPoint (Point a _) = a

pointHash :: Point -> PointHash
pointHash (Point _ b) = b
