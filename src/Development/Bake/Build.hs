{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.Build(ovenIncremental, incrementalDone) where

import Development.Bake.Type
import Development.Shake.Command
import Control.Monad.Extra
import Data.List.Extra
import Control.Arrow
import Data.Function
import System.Directory
import System.IO.Extra
import System.FilePath
import Data.Maybe


ovenIncremental :: Oven state patch test -> Oven state patch test
ovenIncremental oven@Oven{..} = oven
    {ovenUpdateState = \s -> do r <- ovenUpdateState s; whenJust s $ addUpdateState r; return r
    ,ovenPrepare = \s ps -> do incPrepare s ps; ovenPrepare s ps
    }
    where
        showState = stringyTo   ovenStringyState
        readState = stringyFrom ovenStringyState
        showPatch = stringyTo   ovenStringyPatch
        readPatch = stringyFrom ovenStringyPatch

        showUpdate (s1,(s2,ps2)) = show (showState s1, (showState s2, map showPatch ps2))
        readUpdate (read -> (s1,(s2,ps2))) = (readState s1, (readState s2, map readPatch ps2))

        addUpdateState new old =
            appendFile "../incremental-update.txt" $ showUpdate (new,old) ++ "\n"

        readUpdateState = do
            appendFile "../incremental-update.txt" ""
            src <- readFile' "../incremental-update.txt"
            return $ map readUpdate $ lines src

        readCandidate file = do
            state:patches <- fmap lines $ readFile' file
            return (readState state, map readPatch patches)

        incPrepare s ps = do
            dir <- getDirectoryContents ".."
            states <- fmap (map (first showState)) readUpdateState
            let resolve (s,ps) | Just new <- lookup (showState s) states = resolve $ second (++ps) new
                               | otherwise = (showState s, map showPatch ps)
            (selfState, selfPatches) <- return $ resolve (s,ps)

            poss <- fmap catMaybes $ forM [x | x <- dir, "bake-test-" `isPrefixOf` x, takeExtension x == ".incremental"] $ \x -> do
                (state, patches) <- fmap resolve $ readCandidate $ "../" ++ replaceExtension x ".txt"
                return $ if state /= selfState && any (`notElem` selfPatches) patches then Nothing else
                    Just (length $ filter (`notElem` patches) selfPatches, dropExtension x)

            when (not $ null poss) $ do
                let best = snd $ minimumBy (compare `on` fst) poss
                unit $ cmd "cp --preserve --recursive --no-target-directory" ("../" ++ best) "."


incrementalDone :: IO ()
incrementalDone = do
    x <- getCurrentDirectory
    writeFile (x <.> "incremental") ""
