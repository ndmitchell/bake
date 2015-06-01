{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

-- Stuff on disk on the server
module Development.Bake.Server.Database(
    Point, Run,
    DbState(..), DbPatch(..), DbReject(..), DbPoint(..), DbRun(..), DbTest(..),
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

data DbState = DbState
    {sState :: State, sCreate :: UTCTime, sPoint :: Maybe Point}

createState = "CREATE TABLE state (" ++
    "state TEXT NOT NULL UNIQUE PRIMARY KEY, time TEXT NOT NULL, point INTEGER)"

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
    "patch TEXT NOT NULL, test TEXT NOT NULL, run INTEGER)"

data DbPoint = DbPoint
    {tState :: State, tPatches :: String} -- surrounded by /

createPoint = "CREATE TABLE point (" ++
    "state TEXT NOT NULL, patches TEXT NOT NULL, UNIQUE(state, patches))"

data DbRun = DbRun
    {rPoint :: Point, rTest :: Maybe Test, rSuccess :: Bool
    ,rClient :: Client, rStart :: UTCTime, rDuration :: Seconds}

createRun = "CREATE TABLE run (" ++
    "point INTEGER NOT NULL, test TEXT, success INTEGER NOT NULL, " ++
    "client TEXT NOT NULL, start TEXT NOT NULL, duration REAL NOT NULL)"

data DbTest = DbTest
    {tPoint :: Point, tTest :: Maybe Test}
    -- include the Nothing result so that if something has no tests it is still recorded

createTests = "CREATE TABLE test (" ++
    "point INTEGER NOT NULL, test TEXT)"

create :: String -> IO Connection
create file = do
    c <- open file
    execute_ c $ fromString createState
    execute_ c $ fromString createPatch
    execute_ c $ fromString createReject
    execute_ c $ fromString createPoint
    execute_ c $ fromString createRun
    execute_ c $ fromString createTests
    return c

instance FromRow DbPatch where
    fromRow = DbPatch <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DbPatch where
    toRow (DbPatch a b c d e f g h i) = toRow (a,b,c,d,e,f,g,h,i)

instance ToRow DbState where
    toRow (DbState a b c) = toRow (a,b,c)

instance ToRow DbReject where
    toRow (DbReject a b c) = toRow (a,b,c)

instance ToRow DbRun where
    toRow (DbRun a b c d e f) = toRow (a,b,c,d,e,f)

instance FromField Patch where fromField = fmap Patch . fromField
instance ToField Patch where toField = toField . fromPatch
instance FromField State where fromField = fmap State . fromField
instance ToField State where toField = toField . fromState
instance FromField Test where fromField = fmap Test . fromField
instance ToField Test where toField = toField . fromTest
instance FromField Client where fromField = fmap Client . fromField
instance ToField Client where toField = toField . fromClient
