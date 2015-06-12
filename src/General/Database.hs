{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns, RankNTypes, TypeOperators, TypeFamilies, ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module General.Database(
    Pred, (%==), (%==%), (%>), (%<), (%&&), nullP, likeP,
    orderDesc, distinct, limit,
    Upd(..),
    TypeField(..),
    Table, table, Column, column, rowid, norowid,
    sqlInsert, sqlUpdate, sqlSelect, sqlDelete, sqlEnsureTable, sqlUnsafe
    ) where

import Data.List.Extra
import Data.String
import Data.Maybe
import Data.Time.Clock
import Data.Tuple.Extra
import Database.SQLite.Simple hiding ((:=))
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField


type family Uncolumns cs
type instance Uncolumns () = ()
type instance Uncolumns (Column a) = Only a
type instance Uncolumns (Only (Column a)) = Only a
type instance Uncolumns (Column a, Column b) = (a, b)
type instance Uncolumns (Column a, Column b, Column c) = (a, b, c)
type instance Uncolumns (Column a, Column b, Column c, Column d) = (a, b, c, d)
type instance Uncolumns (Column a, Column b, Column c, Column d, Column e) = (a, b, c, d, e)
type instance Uncolumns (Column a, Column b, Column c, Column d, Column e, Column f) = (a, b, c, d, e, f)
type instance Uncolumns (Column a, Column b, Column c, Column d, Column e, Column f, Column g) = (a, b, c, d, e, f, g)
type instance Uncolumns (Column a, Column b, Column c, Column d, Column e, Column f, Column g, Column h) = (a, b, c, d, e, f, g, h)
type instance Uncolumns (Column a, Column b, Column c, Column d, Column e, Column f, Column g, Column h, Column i) = (a, b, c, d, e, f, g, h, i)

data Table rowid cs = Table {tblName :: String, tblKeys :: [Column_], tblCols :: [Column_]}

data Column c = Column {colTable :: String, colName :: String, colSqlType :: String} deriving (Eq,Show)

type Column_ = Column ()

column_ :: Column c -> Column_
column_ Column{..} = Column{..}

class TypeField field where
    typeField :: field -> String

instance TypeField String where typeField _ = "TEXT NOT NULL"
instance TypeField Int where typeField _ = "INTEGER NOT NULL"
instance TypeField Double where typeField _ = "REAL NOT NULL"
instance TypeField UTCTime where typeField _ = "TEXT NOT NULL"
instance TypeField Bool where typeField _ = "INTEGER NOT NULL"
instance TypeField a => TypeField (Maybe a) where
    typeField x | Just s <- stripSuffix " NOT NULL" s = s
                | otherwise = error "Can't remove the NULL constraint"
        where s = typeField $ fromJust x

class Columns cs where columns :: cs -> [Column_]
instance Columns () where columns () = []
instance Columns (Column c1) where columns c1 = [column_ c1]
instance Columns (Only (Column c1)) where columns (Only c1) = [column_ c1]
instance Columns (Column c1, Column c2) where columns (c1, c2) = [column_ c1, column_ c2]
instance Columns (Column c1, Column c2, Column c3) where columns (c1, c2, c3) = [column_ c1, column_ c2, column_ c3]
instance Columns (Column c1, Column c2, Column c3, Column c4) where columns (c1, c2, c3, c4) = [column_ c1, column_ c2, column_ c3, column_ c4]
instance Columns (Column c1, Column c2, Column c3, Column c4, Column c5) where columns (c1, c2, c3, c4, c5) = [column_ c1, column_ c2, column_ c3, column_ c4, column_ c5]
instance Columns (Column c1, Column c2, Column c3, Column c4, Column c5, Column c6) where columns (c1, c2, c3, c4, c5, c6) = [column_ c1, column_ c2, column_ c3, column_ c4, column_ c5, column_ c6]
instance Columns (Column c1, Column c2, Column c3, Column c4, Column c5, Column c6, Column c7) where columns (c1, c2, c3, c4, c5, c6, c7) = [column_ c1, column_ c2, column_ c3, column_ c4, column_ c5, column_ c6, column_ c7]
instance Columns (Column c1, Column c2, Column c3, Column c4, Column c5, Column c6, Column c7, Column c8) where columns (c1, c2, c3, c4, c5, c6, c7, c8) = [column_ c1, column_ c2, column_ c3, column_ c4, column_ c5, column_ c6, column_ c7, column_ c8]
instance Columns (Column c1, Column c2, Column c3, Column c4, Column c5, Column c6, Column c7, Column c8, Column c9) where columns (c1, c2, c3, c4, c5, c6, c7, c8, c9) = [column_ c1, column_ c2, column_ c3, column_ c4, column_ c5, column_ c6, column_ c7, column_ c8, column_ c9]

table :: (Columns keys, Columns cols) => String -> Column rowid -> keys -> cols -> Table rowid (Uncolumns cols)
-- important to produce name before looking at columns
table name rowid (columns -> keys) (columns -> cols) = Table name (check keys) (check cols)
    where
        check x | nubOrd (map colTable $ keys ++ cols) /= [name] = error "Column with the wrong table"
                | not $ null $ map colName keys \\ map colName cols = error "Key column which is not one of the normal columns"
                | colName rowid `notElem` ["","rowid"] = error "Rowid column must have name rowid"
                | otherwise = x

column :: forall c rowid cs . TypeField c => Table rowid cs -> String -> Column c
column tbl row = Column (tblName tbl) row (typeField (undefined :: c))

rowid :: Table rowid cs -> Column rowid
rowid tbl = Column (tblName tbl) "rowid" ""

norowid :: Column ()
norowid = Column "" "" ""

sqlInsert :: (ToRow cs, FromField rowid) => Connection -> Table rowid cs -> cs -> IO rowid
sqlInsert conn tbl val = do
    let vs = toRow val
    -- FIXME: Should combine the last_insert_rowid with the INSERT INTO
    let str = "INSERT INTO " ++ tblName tbl ++ " VALUES (" ++ intercalate "," (replicate (length vs) "?") ++ ")"
    execute conn (fromString str) vs
    [Only row] <- query_ conn (fromString "SELECT last_insert_rowid()")
    return row


sqlUpdate :: Connection -> [Upd] -> [Pred] -> IO ()
sqlUpdate conn upd pred = do
    let (updCs, updVs) = unzip $ map unupdate upd
    let (prdStr, _, prdCs, prdVs) = unpred pred
    let tbl = nubOrd $ map colTable $ updCs ++ prdCs
    case tbl of
        _ | null upd -> fail "Must update at least one column"
        [t] -> do
            let str = "UPDATE " ++ t ++ " SET " ++ intercalate ", " (map ((++ "=?") . colName) updCs) ++ " WHERE " ++ prdStr
            execute conn (fromString str) (updVs ++ prdVs)
        _ -> fail "Must update all in the same column"


sqlDelete :: Connection -> Table rowid cs -> [Pred] -> IO ()
sqlDelete conn tbl pred = do
    let (prdStr, _, prdCs, prdVs) = unpred pred
    case nubOrd $ tblName tbl : map colTable prdCs of
        [t] -> do
            let str = "DELETE FROM " ++ t ++ " WHERE " ++ prdStr
            execute conn (fromString str) prdVs
        ts -> fail $ "sqlDelete, can only delete from one table but you are touching: " ++ unwords ts


sqlSelect :: (FromRow (Uncolumns cs), Columns cs) => Connection -> cs -> [Pred] -> IO [Uncolumns cs]
sqlSelect conn cols pred = do
    let outCs = columns cols
    let (prdStr, prdDs, prdCs, prdVs) = unpred pred
    let str = "SELECT " ++ intercalate ", " [(if c `elem` prdDs then "DISTINCT " else "") ++ colTable ++ "." ++ colName | c@Column{..} <- outCs] ++ " " ++
              "FROM " ++ intercalate ", " (nubOrd $ map colTable $ outCs ++ prdCs) ++ " WHERE " ++ prdStr
    query conn (fromString str) prdVs


sqlEnsureTable :: Connection -> Table rowid cs -> IO ()
sqlEnsureTable conn Table{..} = do
    let fields = intercalate ", " $
            [colName ++ " " ++ colSqlType | Column{..} <- tblCols] ++
            ["PRIMARY KEY (" ++ intercalate ", " (map colName tblKeys) ++ ")" | not $ null tblKeys]
    let str = "CREATE TABLE IF NOT EXISTS " ++ tblName ++ "(" ++ fields ++ ")"
    existing <- query conn (fromString "SELECT sql FROM sqlite_master WHERE type = ? AND name = ?") ("table", tblName)
    case existing of
        [Only s] | str == s -> return ()
        [] -> execute_ conn $ fromString str
        _ -> error $ "Trying to ensure table " ++ tblName ++ " but mismatch" ++
                     "\nCreating:\n" ++ str ++ "\nGot:\n" ++ unlines (map fromOnly existing)


sqlUnsafe :: (ToRow q, FromRow r) => Connection -> String -> q -> IO [r]
sqlUnsafe conn str q = query conn (fromString str) q


data Upd = forall a . ToField a => Column a := a

unupdate :: Upd -> (Column_, SQLData)
unupdate (c := v) = (column_ c, toField v)

data Pred
    = PNull Column_
    | PEq Column_ SQLData
    | PGt Column_ SQLData
    | PLt Column_ SQLData
    | PEqP Column_ Column_
    | PLike Column_ SQLData
    | PAnd [Pred]
    | PDistinct Column_
    | POrder Column_ String
    | PLimit Int

distinct :: Column c -> Pred
distinct c = PDistinct (column_ c)

limit :: Int -> Pred
limit = PLimit

orderDesc :: Column UTCTime -> Pred
orderDesc c = POrder (column_ c) $ colTable c ++ "." ++ colName c ++ " DESC"

nullP :: Column (Maybe c) -> Pred
nullP c = PNull (column_ c)

likeP :: ToField c => Column c -> c -> Pred
likeP (column_ -> c) (toField -> v) = PLike c v

(%&&) :: Pred -> Pred -> Pred
(%&&) a b = PAnd [a,b]

(%==) :: ToField c => Column c -> c -> Pred
(%==) (column_ -> c) (toField -> v)
    | v == SQLNull = PNull c
    | otherwise = PEq c v

(%>) :: ToField c => Column c -> c -> Pred
(%>) (column_ -> c) (toField -> v)
    | v == SQLNull = error $ "Can't %> on a NULL"
    | otherwise = PGt c v

(%<) :: ToField c => Column c -> c -> Pred
(%<) (column_ -> c) (toField -> v)
    | v == SQLNull = error $ "Can't %> on a NULL"
    | otherwise = PLt c v

(%==%) :: ToField c => Column c -> Column c -> Pred
(%==%) c1 c2
    | isNull c1 || isNull c2 = error $ show ("Column must be NOT NULL to do %==%", show c1, show c2)
    | otherwise = PEqP (column_ c1) (column_ c2)
    where isNull c = not $ colSqlType c == "" || " NOT NULL" `isSuffixOf` colSqlType c

unpred :: [Pred] -> (String, [Column_], [Column_], [SQLData])
unpred ps =
    let (a,b,c) = f $ PAnd pred
    in (a ++ (if null order then "" else " ORDER BY " ++ unwords [x | POrder _ x <- order]) ++
             (if null limit then "" else " LIMIT " ++ head [show x | PLimit x <- limit])
                ,
       [x | PDistinct x <- dist], b ++ [x | POrder x _ <- order], c)
    where
        isDistinct PDistinct{} = True; isDistinct _ = False
        isOrder POrder{} = True; isOrder _ = False
        isLimit PLimit{} = True; isLimit _ = False
        (dist, (order, (limit, pred))) = second (second (partition isLimit) . partition isOrder) $ partition isDistinct ps

        g Column{..} = colTable ++ "." ++ colName

        f (PNull c) = (g c ++ " IS NULL", [c], [])
        f (PEq c v) = (g c ++ " = ?", [c], [v]) -- IS always works, but is a LOT slower
        f (PGt c v) = (g c ++ " > ?", [c], [v]) -- IS always works, but is a LOT slower
        f (PLt c v) = (g c ++ " < ?", [c], [v]) -- IS always works, but is a LOT slower
        f (PEqP c1 c2) = (g c1 ++ " = " ++ g c2, [c1,c2], [])
        f (PLike c v) = (g c ++ " LIKE ?", [c], [v])
        f (PAnd []) = ("NULL IS NULL", [], [])
        f (PAnd [x]) = f x
        f (PAnd xs) = (intercalate " AND " ["(" ++ s ++ ")" | s <- ss], concat cs, concat vs)
            where (ss,cs,vs) = unzip3 $ map f xs
        f _ = error "Unrecognised Pred"

instance FromField () where
    fromField _ = return ()
