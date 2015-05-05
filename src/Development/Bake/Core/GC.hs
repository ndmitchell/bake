
-- | Define a continuous integration system.
module Development.Bake.Core.GC(garbageCollect) where

import Control.Exception.Extra
import General.Extra
import System.Directory.Extra
import System.FilePath
import Control.Monad.Extra
import Control.Applicative
import Data.Either.Extra
import Data.List.Extra
import System.Time.Extra
import Data.Maybe
import System.DiskSpace
import Prelude


-- | Garbage collect enough files to satisfy the requirements.
garbageCollect
    :: Integer -- ^ Minimum number of bytes you want free on the drive (use 0 if you don't want any)
    -> Double -- ^ Ratio of the drive you want free, e.g. 0.25 to demand a quarter of the drive free (1 to delete everything you can)
    -> Seconds -- ^ Minimum age to delete in seconds
    -> [FilePath] -- ^ Directories containing Bake stuff
    -> IO ()
garbageCollect _ _ _ [] = return ()
garbageCollect bytes ratio limit dirs@(d:_) = do
    total <- diskTotal <$> getDiskUsage d
    gs <- reverse . sortOn gAge . filter ((>= limit) . gAge) <$> garbageQuery dirs

    bytes <- return $ max (floor $ fromIntegral total * ratio) bytes

    flip loopM gs $ \gs -> case gs of
        [] -> return $ Right ()
        g:gs -> do
            b <- getAvailSpace d
            if b >= bytes then
                return $ Right ()
            else do
                putStr $ "Deleting " ++ gPath g ++ "..."
                res <- try_ $
                    if gDirectory g then do
                        renameDirectory (gPath g) (gPath g <.> "gc")
                        removeDirectoryRecursive (gPath g <.> "gc")
                    else
                        removeFile (gPath g)
                putStrLn $ either (\e -> "FAILED\n" ++ show e) (const "success") res
                return $ Left gs
    putStrLn "[Bake] Disk space garbage collection complete"


data Garbage = Garbage
    {gPath :: FilePath
    ,gDirectory :: Bool
    ,gAge :: Seconds -- ^ Age in seconds, will be positive (unless clock adjustments)
    }


-- | Given a list of directories, find the possible garbage present.
garbageQuery :: [FilePath] -> IO [Garbage]
garbageQuery dirs = do
    now <- getCurrentTime
    let f gen file = fmap eitherToMaybe $ try_ $ do
            t <- getModificationTime file
            return $ gen $ now `subtractTime` t

    fmap (concatMap catMaybes) $ forM dirs $ \dir -> do
        dirs <- listContents dir
        dirs <- forM dirs $ \dir -> f (Garbage dir True) $ dir </> ".bake.name"

        files <- listFiles (dir </> "bake-string") `catch_` const (return [])
        files <- forM files $ \file -> f (Garbage file False) file
        return $ dirs ++ files
