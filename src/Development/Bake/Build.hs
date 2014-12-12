{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.Build(ovenIncremental, incrementalDone) where

import Development.Bake.Core.Type
import Development.Shake.Command
import Control.Monad.Extra
import Control.Applicative
import System.FilePath
import Control.Exception.Extra
import System.Directory


-- Files involved:
-- ../bake-incremental.txt, stores the directory name of the most recent successful increment
-- .bake.incremental exists if you have done an increment yourself, or copied from someone who has
-- we always use the most recent increment to build onwards from

-- | This requires a version of @cp@. On Windows, you can get that here:
--   <http://gnuwin32.sourceforge.net/packages/coreutils.htm>
ovenIncremental :: Oven state patch test -> Oven state patch test
ovenIncremental oven@Oven{..} = oven{ovenPrepare = \s ps -> do incPrepare s ps; ovenPrepare s ps}
    where
        incPrepare s ps = ignore $ do
            -- if i have already been incremental'd (via copy, or via completion) don't do anything
            unlessM (doesFileExist ".bake.incremental") $ do
                src <- takeWhile (/= '\n') <$> readFile "../bake-incremental.txt"
                whenM (doesFileExist $ ".." </> src </> ".bake.incremental") $ do
                    putStrLn $ "Preparing by copying from " ++ src
                    unit $ cmd "cp --preserve=timestamps --recursive --no-target-directory" ("../" ++ src) "."

incrementalStart :: IO ()
incrementalStart =
    writeFile ".bake.incremental" ""

incrementalDone :: IO ()
incrementalDone = do
    incrementalStart
    x <- getCurrentDirectory
    writeFile "../bake-incremental.txt" $ unlines [takeFileName x]
