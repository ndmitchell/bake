{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

-- Stuff on disk on the server
module Development.Bake.Server.Database(
    PointId, RunId, StateId, PatchId, patchIds, fromPatchIds, patchIdsSuperset,
    DbPatch(..), DbReject(..), DbPoint(..), DbRun(..),
    stTable, stId, stState, stCreate, stPoint, stDuration,
    skTable, skTest, skComment,
    tsTable, tsPoint, tsTest,
    create
    ) where

import Development.Bake.Core.Type
import Control.Applicative
import Data.String
import General.Extra
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import System.Time.Extra
import Data.List.Extra
import General.Database
import Prelude


newtype PointId = PointId Int deriving (ToField, FromField)
newtype RunId = RunId Int deriving (ToField, FromField, Eq)
newtype StateId = StateId Int deriving (ToField, FromField)
newtype PatchId = PatchId Int deriving (ToField, FromField)

instance Show PointId where show (PointId x) = "point-" ++ show x
instance Show RunId where show (RunId x) = "run-" ++ show x
instance Show StateId where show (StateId x) = "state-" ++ show x
instance Show PatchId where show (PatchId x) = "patch-" ++ show x

instance Read RunId where readsPrec i s = [x | Just s <- [stripPrefix "run-" s], x <- readsPrec i s]

newtype PatchIds = PatchIds String deriving (ToField, FromField)

patchIds :: [PatchId] -> PatchIds
patchIds = PatchIds . concatMap (\(PatchId x) -> "[" ++ show x ++ "]")

patchIdsSuperset :: [PatchId] -> PatchIds
patchIdsSuperset = PatchIds . ('%':) . concatMap (\(PatchId x) -> "[" ++ show x ++ "]%")

fromPatchIds :: PatchIds -> [PatchId]
fromPatchIds (PatchIds "") = []
fromPatchIds (PatchIds xs) = map (PatchId . read) $ splitOn "][" $ init $ tail xs


stTable = table "state" stId stState (stState,stCreate,stPoint,stDuration)
stId = rowid stTable :: Column StateId
stState = column stTable "state" "TEXT NOT NULL" :: Column State
stCreate = column stTable "time" "TEXT NOT NULL" :: Column UTCTime
stPoint = column stTable "point" "INTEGER" :: Column (Maybe PointId)
stDuration = column stTable "duration" "REAL NOT NULL" :: Column Seconds


data DbPatch = DbPatch
    {pPatch :: Patch, pAuthor :: String, pQueue :: UTCTime
    ,pStart :: Maybe UTCTime, pDelete :: Maybe UTCTime, pSupersede :: Maybe UTCTime, pReject :: Maybe UTCTime
    ,pPlausible :: Maybe UTCTime, pMerge :: Maybe UTCTime}

createPatch = "CREATE TABLE IF NOT EXISTS patch (" ++
    "patch TEXT NOT NULL UNIQUE PRIMARY KEY, author TEXT NOT NULL, queue TEXT NOT NULL, " ++
    "start TEXT, delete_ TEXT, supersede TEXT, reject TEXT, plausible TEXT, merge TEXT)"

data DbReject = DbReject
    {jPatch :: PatchId, jTest :: Maybe Test, jRun :: RunId}

createReject = "CREATE TABLE IF NOT EXISTS reject (" ++
    "patch INTEGER NOT NULL, test TEXT, run INTEGER NOT NULL)"

data DbPoint = DbPoint
    {tState :: StateId, tPatches :: PatchIds} -- surrounded by /

createPoint = "CREATE TABLE IF NOT EXISTS point (" ++
    "state INTEGER NOT NULL, patches TEXT NOT NULL, PRIMARY KEY (state, patches))"

data DbRun = DbRun
    {rPoint :: PointId, rTest :: Maybe Test, rSuccess :: Bool
    ,rClient :: Client, rStart :: UTCTime, rDuration :: Seconds}

createRun = "CREATE TABLE IF NOT EXISTS run (" ++
    "point INTEGER NOT NULL, test TEXT, success INTEGER NOT NULL, " ++
    "client TEXT NOT NULL, start TEXT NOT NULL, duration REAL NOT NULL)"

tsTable = table "test" norowid () (tsPoint, tsTest)
tsPoint = column tsTable "point" "INTEGER NOT NULL" :: Column PointId
tsTest = column tsTable "test" "TEXT" :: Column (Maybe Test)

skTable = table "skip" norowid skTest (skTest, skComment)
skTest = column skTable "test" "TEXT NOT NULL" :: Column Test
skComment = column skTable "comment" "TEXT NOT NULL" :: Column String

create :: String -> IO Connection
create file = do
    c <- open file
    sqlCreateNotExists c stTable
    execute_ c $ fromString createPatch
    execute_ c $ fromString createReject
    execute_ c $ fromString createPoint
    execute_ c $ fromString createRun
    sqlCreateNotExists c tsTable
    sqlCreateNotExists c skTable
    return c

instance FromRow DbPatch where
    fromRow = DbPatch <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow DbPoint where
    fromRow = DbPoint <$> field <*> field

instance ToRow DbPatch where
    toRow (DbPatch a b c d e f g h i) = toRow (a,b,c,d,e,f,g,h,i)

instance ToRow DbReject where
    toRow (DbReject a b c) = toRow (a,b,c)

instance ToRow DbRun where
    toRow (DbRun a b c d e f) = toRow (a,b,c,d,e,f)

instance FromRow DbRun where
    fromRow = DbRun <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DbPoint where
    toRow (DbPoint a b) = toRow (a,b)
