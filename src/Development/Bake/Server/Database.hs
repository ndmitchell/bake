{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

-- Stuff on disk on the server
module Development.Bake.Server.Database(
    Point, Run,
    DbPatch(..), DbReject(..), DbPointState(..), DbPointPatch(..), DbRun(..), DbTests(..),
    create
    ) where

import Development.Bake.Core.Type
import Data.String
import General.Extra
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Control.Applicative
import System.Time.Extra


newtype Point = Point Int deriving (ToField, FromField)

newtype Run = Run Int deriving (ToField, FromField)

data DbPatch = DbPatch
    {pPatch :: Patch, pAuthor :: String, pQueue :: UTCTime
    ,pStart :: Maybe UTCTime, pDelete :: Maybe UTCTime, pSupersede :: Maybe UTCTime, pReject :: Maybe UTCTime
    ,pPlausible :: Maybe UTCTime, pMerge :: Maybe UTCTime}

createPatch = "CREATE TABLE patch (" ++
    "patch TEXT NOT NULL UNIQUE PRIMARY KEY, author TEXT NOT NULL, queue TEXT NOT NULL, " ++
    "start TEXT, delete_ TEXT, supersede TEXT, reject TEXT, plausible TEXT, merge TEXT)"

data DbReject = DbReject
    {jPatch :: Patch, jTest :: Maybe Test, jRun :: Run}

createReject = "CREATE TABLE reject (" ++
    "patch TEXT NOT NULL, test TEXT NOT NULL, point INTEGER NOT NULL, run INTEGER)"

data DbPointState = DbPointState
    {psPoint :: Point, psState :: State}

createPointState = "CREATE TABLE point_state (" ++
    "point INTEGER NOT NULL UNIQUE PRIMARY KEY, state TEXT NOT NULL)"

data DbPointPatch = DbPointPatch
    {ppPoint :: Point, ppPatch :: Patch, ppIndex :: Int}

createPointPatch = "CREATE TABLE point_patch (" ++
    "point INTEGER NOT NULL, patch TEXT NOT NULL, index_ INTEGER NOT NULL)"

data DbRun = DbRun
    {rRun :: Run, rPoint :: Point, rTest :: Test
    ,rClient :: Client, rStart :: UTCTime, rDuration :: Seconds}

createRun = "CREATE TABLE run (" ++
    "run INTEGER NOT NULL UNIQUE PRIMARY KEY, point INTEGER NOT NULL, test TEXT NOT NULL, " ++
    "client TEXT NOT NULL, start TEXT NOT NULL, duration REAL NOT NULL)"

data DbTests = DbTests
    {tPoint :: Point, tTest :: Test}

createTests = "CREATE TABLE tests (" ++
    "point INTEGER NOT NULL, test TEXT NOT NULL)"

create :: String -> IO Connection
create file = do
    c <- open file
    execute_ c $ fromString createPatch
    execute_ c $ fromString createReject
    execute_ c $ fromString createPointState
    execute_ c $ fromString createPointPatch
    execute_ c $ fromString createRun
    execute_ c $ fromString createTests
    return c

instance FromRow DbPatch where
    fromRow = DbPatch <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DbPatch where
    toRow (DbPatch a b c d e f g h i) = toRow (a,b,c,d,e,f,g,h,i)

instance FromField Patch where fromField = fmap Patch . fromField
instance ToField Patch where toField = toField . fromPatch
