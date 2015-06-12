{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

-- Stuff on disk on the server
module Development.Bake.Server.Database(
    PointId, RunId, StateId, PatchId, patchIds, fromPatchIds, patchIdsSuperset,
    stTable, stId, stState, stCreate, stPoint, stDuration,
    pcTable, pcId, pcPatch, pcAuthor, pcQueue, pcStart, pcDelete, pcSupersede, pcReject, pcPlausible, pcMerge,
    rjTable, rjPatch, rjTest, rjRun,
    ptTable, ptId, ptState, ptPatches,
    skTable, skTest, skComment,
    tsTable, tsPoint, tsTest,
    rnTable, rnId, rnPoint, rnTest, rnSuccess, rnClient, rnStart, rnDuration,
    create, save
    ) where

import Development.Bake.Core.Type
import Data.String
import Control.Exception
import General.Extra
import qualified Database.SQLite3 as SQ
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import System.Time.Extra
import Data.Hashable
import Data.List.Extra
import Control.Monad
import Data.Maybe
import Safe
import General.Database
import Prelude


newtype PointId = PointId Int deriving (ToField, FromField, TypeField, Eq, Hashable)
newtype RunId = RunId Int deriving (Eq, ToField, FromField, TypeField)
newtype StateId = StateId Int deriving (ToField, FromField, TypeField)
newtype PatchId = PatchId Int deriving (ToField, FromField, TypeField)

instance Show PointId where show (PointId x) = "point-" ++ show x
instance Show RunId where show (RunId x) = "run-" ++ show x
instance Show StateId where show (StateId x) = "state-" ++ show x
instance Show PatchId where show (PatchId x) = "patch-" ++ show x

instance Read RunId where readsPrec i s = [x | Just s <- [stripPrefix "run-" s], x <- readsPrec i s]

newtype PatchIds = PatchIds String deriving (ToField, FromField, TypeField)

patchIds :: [PatchId] -> PatchIds
patchIds = PatchIds . concatMap (\(PatchId x) -> "[" ++ show x ++ "]")

patchIdsSuperset :: [PatchId] -> PatchIds
patchIdsSuperset = PatchIds . ('%':) . concatMap (\(PatchId x) -> "[" ++ show x ++ "]%")

fromPatchIds :: PatchIds -> [PatchId]
fromPatchIds (PatchIds "") = []
fromPatchIds (PatchIds xs) = map (PatchId . readNote "fromPatchIds") $ splitOn "][" $ init $ tail xs


stTable = table "state" stId stState (stState,stCreate,stPoint,stDuration)
stId = rowid stTable :: Column StateId
stState = column stTable "state" :: Column State
stCreate = column stTable "time" :: Column UTCTime
stPoint = column stTable "point" :: Column (Maybe PointId)
stDuration = column stTable "duration" :: Column Seconds

pcTable = table "patch" pcId pcPatch (pcPatch, pcAuthor, pcQueue, pcStart, pcDelete, pcSupersede, pcReject, pcPlausible, pcMerge)
pcId = rowid pcTable :: Column PatchId
pcPatch = column pcTable "patch" :: Column Patch
pcAuthor = column pcTable "author" :: Column String
pcQueue = column pcTable "queue" :: Column UTCTime
pcStart = column pcTable "start" :: Column (Maybe UTCTime)
pcDelete = column pcTable "delete_" :: Column (Maybe UTCTime)
pcSupersede = column pcTable "supersede" :: Column (Maybe UTCTime)
pcReject = column pcTable "reject" :: Column (Maybe UTCTime)
pcPlausible = column pcTable "plausible" :: Column (Maybe UTCTime)
pcMerge = column pcTable "merge" :: Column (Maybe UTCTime)

rjTable = table "reject" norowid () (rjPatch, rjTest, rjRun)
rjPatch = column rjTable "patch" :: Column PatchId
rjTest = column rjTable "test" :: Column (Maybe Test)
rjRun = column rjTable "run" :: Column RunId

ptTable = table "point" ptId (ptState, ptPatches) (ptState, ptPatches)
ptId = rowid ptTable :: Column PointId
ptState = column ptTable "state" :: Column StateId
ptPatches = column ptTable "patches" :: Column PatchIds

rnTable = table "run" rnId () (rnPoint, rnTest, rnSuccess, rnClient, rnStart, rnDuration)
rnId = rowid rnTable :: Column RunId
rnPoint = column rnTable "point" :: Column PointId
rnTest = column rnTable "test" :: Column (Maybe Test)
rnSuccess = column rnTable "success" :: Column Bool
rnClient = column rnTable "client" :: Column Client
rnStart = column rnTable "start" :: Column UTCTime
rnDuration = column rnTable "duration" :: Column Seconds

tsTable = table "test" norowid () (tsPoint, tsTest)
tsPoint = column tsTable "point" :: Column PointId
tsTest = column tsTable "test" :: Column (Maybe Test)

skTable = table "skip" norowid skTest (skTest, skComment)
skTest = column skTable "test" :: Column Test
skComment = column skTable "comment" :: Column String

create :: Maybe FilePath -> IO Connection
create file = do
    conn <- open $ fromMaybe ":memory:" file
    execute_ conn $ fromString "PRAGMA journal_mode = WAL;"
    execute_ conn $ fromString "PRAGMA synchronous = OFF;"
    sqlEnsureTable conn stTable
    sqlEnsureTable conn pcTable
    sqlEnsureTable conn rjTable
    sqlEnsureTable conn ptTable
    sqlEnsureTable conn rnTable
    sqlEnsureTable conn tsTable
    sqlEnsureTable conn skTable
    return conn

save :: Connection -> FilePath -> IO ()
save conn file = void $ bracket (open file) close $ \dest -> bracket
    (SQ.backupInit (connectionHandle dest) (fromString "main") (connectionHandle conn) (fromString "main"))
    SQ.backupFinish $
    \b -> SQ.backupStep b (-1)
