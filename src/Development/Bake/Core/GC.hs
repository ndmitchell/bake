
-- | Define a continuous integration system.
module Development.Bake.Core.GC(garbageCollect, garbageQuery) where

import Control.Exception.Extra
import General.Extra
import System.Directory.Extra
import System.FilePath
import Control.Monad.Extra
import Control.Applicative
import Data.Either.Extra
import Data.List.Extra
import System.Time.Extra
import Prelude



garbageCollect :: Bool -> Double -> [FilePath] -> IO ()
garbageCollect dry_run days dirs = do
    xs <- concatMapM (garbageQuery $ days * 24 * 60 *60) $ if null dirs then ["."] else dirs
    failed <- flip filterM xs $ \x -> do
        (act,msg) <- return $ case x of
            Left file -> (removeFile file, "Delete file " ++ file)
            Right dir -> (removeDirectoryRecursive dir, "Delete directory " ++ dir)
        if dry_run then do
            putStrLn $ "[DRY RUN] " ++ msg
            return False
         else do
            putStr $ msg ++ "... "
            res <- isRight <$> try_ act
            putStrLn $ if res then "success" else "FAILED"
            return $ not res
    putStrLn $
        (if dry_run then "[DRY RUN] " else "") ++
        "Deleted " ++ show (length xs) ++ " items, " ++ show (length failed) ++ " failed"


-- | Either a Left file, or Right dir
garbageQuery :: Double -> FilePath -> IO [Either FilePath FilePath]
garbageQuery secs dir = do
    now <- getCurrentTime
    let test file = do
            t <- getModificationTime file
            return $ now `subtractTime` t > secs

    dirs <- listContents dir
    dirs <- flip filterM dirs $ \dir -> do
        let file = dir </> ".bake.name"
        doesDirectoryExist dir &&^ doesFileExist file &&^ do test file

    files <- filterM test =<< ifM (doesDirectoryExist $ dir </> "bake-string") (listFiles $ dir </> "bake-string") (return [])
    return $ map Left files ++ map Right dirs
