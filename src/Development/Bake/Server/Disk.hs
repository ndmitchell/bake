{-# LANGUAGE GADTs, ViewPatterns, RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- Stuff on disk on the server
module Development.Bake.Server.Disk(
    Point, Run,
    Disk, newDisk,
    Q(..), load, save, notify
    ) where

import Control.Exception.Extra
import Control.Concurrent.Extra
import Development.Bake.Core.Type
import Data.IORef
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Data.Time
import System.Time.Extra
import System.FilePath
import Control.Monad.Extra
import System.Directory.Extra
import Data.Tuple.Extra
import Data.Unique


newtype Run = Run {fromRun :: Int}

newtype Point = Point {fromPoint :: Int} deriving (Eq, Ord)

data Q result where
    SkipList :: Q [Test]
    SkipAuthor :: Test -> Q (Maybe String)

    StateExtraShort :: State -> Q T.Text
    StateExtraLong :: State -> Q FilePath
    StatePrevious :: State -> Q (Maybe (State, [Patch]))
    StateDuration :: State -> Q (UTCTime, Seconds)
    StateOutputs :: State -> Q FilePath

    PatchList :: Q [Patch]
    PatchExtraShort :: Patch -> Q T.Text
    PatchExtraLong :: Patch -> Q FilePath
    PatchAuthor :: Patch -> Q String
    PatchSubmitted :: Patch -> Q UTCTime
    PatchRejected :: Patch -> Q [(UTCTime, Point, Maybe Test, Run)]
    PatchStarted :: Patch -> Q (Maybe UTCTime)
    PatchDeleted :: Patch -> Q (Maybe UTCTime)
    PatchSuperseded :: Patch -> Q (Maybe UTCTime)
    PatchPlausible :: Patch -> Q (Maybe UTCTime)
    PatchMerged :: Patch -> Q (Maybe UTCTime)

    PointList :: Q [Point]
    PointResolve :: Point -> Q (State, [Patch])
    PointTests :: Point -> Q (Maybe [Test])

    RunList :: Point -> Maybe Test -> Q [Run]
    RunSuccess :: Point -> Maybe Test -> Run -> Q Bool
    RunDuration :: Point -> Maybe Test -> Run -> Q (UTCTime, Seconds)
    RunClient :: Point -> Maybe Test -> Run -> Q Client
    RunOutputs :: Point -> Maybe Test -> Run -> Q FilePath

newtype Notify = Notify (forall result . Q result -> result -> IO ())

{-# NOINLINE notifyId #-}
notifyId :: IORef Int
notifyId = unsafePerformIO $ newIORef 0

data Disk = Disk Unique FilePath (Var (Map.Map Int Notify))

instance Eq Disk where
    Disk u1 _ _ == Disk u2 _ _ = u1 == u2

newDisk :: FilePath -> IO Disk
newDisk file = do
    v <- newVar Map.empty
    u <- newUnique
    return $ Disk u file v

op :: FilePath -> Q result -> (IO result, result -> IO ())
op dir x = case x of
    SkipList -> listing (\_ files -> map Test files) (dir </> "skip")
    SkipAuthor t -> fileMay $ dir </> "skip" </> fromTest t

    StateExtraShort s -> fileDef T.empty $ state s </> "short.html"
    StateExtraLong s -> constant $ state s </> "long.html"
    StatePrevious s -> fileMay $ state s </> "previous.txt"
    StateDuration s -> file $ state s </> "duration.txt"
    StateOutputs s -> constant $ state s </> "outputs"

    PatchList -> listing (\dirs _ -> map Patch dirs) (dir </> "patch")
    PatchExtraShort p -> fileDef T.empty $ patch p </> "short.html"
    PatchExtraLong p -> constant $ patch p </> "long.html"
    PatchAuthor p -> fileDef "Unknown" $ patch p </> "author.txt"
    PatchSubmitted p -> file $ patch p </> "submitted.txt"
    PatchRejected p -> fileDef [] $ patch p </> "rejected.txt"
    PatchStarted p -> fileMay $ patch p </> "started.txt"
    PatchDeleted p -> fileMay $ patch p </> "deleted.txt"
    PatchSuperseded p -> fileMay $ patch p </> "superseded.txt"
    PatchPlausible p -> fileMay $ patch p </> "plausible.txt"
    PatchMerged p -> fileMay $ patch p </> "merged.txt"

    PointList -> listing (\dirs _ -> map (Point . read) dirs) $ dir </> "point"
    PointResolve i -> file $ point i </> "point.txt"
    PointTests i -> fileMay $ point i </> "tests.txt"

    RunList i t -> listing (\dirs _ -> map (Run . read) dirs) $ test i t
    RunSuccess i t r -> file $ run i t r </> "success.txt"
    RunDuration i t r -> file $ run i t r </> "duration.txt"
    RunClient i t r -> file $ run i t r </> "client.txt"
    RunOutputs i t r -> constant $ run i t r </> "outputs"
    where
        state (State s) = dir </> "state" </> s
        patch (Patch p) = dir </> "patch" </> p
        point (Point i) = dir </> "point" </> show i
        test i t = point i </> maybe "_" fromTest t
        run i t (Run r) = test i t </> show r


constant :: result -> (IO result, result -> IO ())
constant x = (return x, error "Cannot set constant")

listing :: ([FilePath] -> [FilePath] -> result) -> FilePath -> (IO result, result -> IO ())
listing op dir = (list, error "Cannot set listing")
    where
        list = do
            xs <- listContents dir
            (dirs,files) <- partitionM doesDirectoryExist xs
            return $ op (map takeFileName dirs) (map takeFileName files)

file :: Stored result => FilePath -> (IO result, result -> IO ())
file x = (storedLoad x, storedSave x)

fileDef :: Stored result => result -> FilePath -> (IO result, result -> IO ())
fileDef def x = (ifM (doesFileExist x) (storedLoad x) (return def), storedSave x)

fileMay :: Stored result => FilePath -> (IO (Maybe result), Maybe result -> IO ())
fileMay x = (ifM (doesFileExist x) (Just <$> storedLoad x) (return Nothing)
            ,maybe (ignore $ removeFile x) (storedSave x))

-- caches with cache invalidation
load :: Disk -> Q result -> IO result
load (Disk _ dir _) q = fst $ op dir q

save :: Disk -> Q result -> result -> IO ()
save (Disk _ dir notify) q a = do
    snd (op dir q) a
    notify <- readVar notify
    forM_ (Map.toAscList notify) $ \(_, Notify act) -> act q a

notify :: Disk -> (forall result . Q result -> result -> IO ()) -> IO (IO ())
notify (Disk _ dir notify) act = do
    u <- atomicModifyIORef notifyId (succ &&& succ)
    modifyVar_ notify $ return . Map.insert u (Notify act)
    return $ modifyVar_ notify $ return . Map.delete u


---------------------------------------------------------------------
-- PUT A VALUE IN A FILE

class Stored a where
    storedSave :: FilePath -> a -> IO ()
    storedLoad :: FilePath -> IO a

instance Stored T.Text where
    storedSave = T.writeFile
    storedLoad = T.readFile

instance Stored String where
    storedSave = writeFile
    storedLoad = readFile

instance Stored UTCTime where
    storedSave file = storedSave file . show
    storedLoad file = read <$> storedLoad file

instance Stored Bool where
    storedSave file = storedSave file . show
    storedLoad file = read <$> storedLoad file

instance Stored (UTCTime, Seconds) where
    storedSave file = storedSave file . show
    storedLoad file = read <$> storedLoad file

instance Stored Client where
    storedSave file = storedSave file . fromClient
    storedLoad file = Client <$> storedLoad file

instance Stored [Test] where
    storedSave file = storedSave file . unlines . map fromTest
    storedLoad file = map Test . lines <$> storedLoad file

instance Stored (State, [Patch]) where
    storedSave file (s,ps) = storedSave file $ unlines $ fromState s : map fromPatch ps
    storedLoad file = do s:ps <- lines <$> storedLoad file; return (State s, map Patch ps)

instance Stored [(UTCTime, Point, Maybe Test, Run)] where
    storedSave file = storedSave file . show . map (\(a,b,c,d) -> (a,fromPoint b,fmap fromTest c,fromRun d))
    storedLoad file = map (\(a,b,c,d) -> (a,Point b,fmap Test c,Run d)) . read <$> storedLoad file

