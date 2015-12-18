
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
    gs <-  reverse . sortOn gAge . filter ((>= limit) . gAge) <$> garbageQuery dirs

    bytes <- return $ max (floor $ fromIntegral total * ratio) bytes

    done <- flip loopM (False,gs) $ \(done,gs) -> case gs of
        [] -> return $ Right done
        g:gs -> do
            b <- getAvailSpace d
            if b >= bytes then
                return $ Right done
            else do
                putStr $ "[BAKE-GC] Deleting " ++ gPath g ++ "..."
                res <- try_ $ do
                    renameDirectory (gPath g) (gPath g <.> "gc")
                    removeDirectoryRecursive (gPath g <.> "gc")
                putStrLn $ either (\e -> "FAILED\n" ++ show e) (const "success") res
                return $ Left (True,gs)
    when done $ do
        totalAfterGC <- diskTotal <$> getDiskUsage d
        putStrLn $ "[BAKE-GC] Disk space garbage collection complete, collected " ++ show (total - totalAfterGC)


data Garbage = Garbage
    {gPath :: FilePath
    ,gAge :: Seconds -- ^ Age in seconds, will be positive (unless clock adjustments)
    } deriving (Show)


-- | Given a list of directories, find the possible garbage present.
garbageQuery :: [FilePath] -> IO [Garbage]
garbageQuery dirs = do
    now <- getCurrentTime
    
    let preserveStore file = takeFileName file /= "bake-store"
            
    let age garbage file = fmap eitherToMaybe $ try_ $ do
            t <- getModificationTime file
            return $ garbage $ now `subtractTime` t
    
    fmap (concatMap catMaybes) $ forM dirs $ \dir -> do
      let inc = dir </> "bake-incremental.txt"
      exist <- doesFileExist inc
      preserveIncremental <- if exist
                             then do
                               incDir <- takeWhile (/= '\n') <$> readFile inc
                               return (preserveIncrementalDirs incDir)
                             else return $ const True
      dirs <- (filter preserveStore . filter preserveIncremental) <$> listContents dir
      forM dirs $ \dir -> age (Garbage dir) $ dir </> ".bake.name"

-- | Compute all dirs pertaining to a specific test run
--
-- Given a baseName of `bake-test-XYZ` this returns a filter for `xxx-XYZ`.
preserveIncrementalDirs :: String -> FilePath -> Bool
preserveIncrementalDirs baseName = let [_,_,testNumber] = splitOn "-" baseName
                                   in \ dir -> testNumber `isSuffixOf` takeFileName dir 
