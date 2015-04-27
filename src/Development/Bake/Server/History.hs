{-# LANGUAGE RecordWildCards, ViewPatterns, ScopedTypeVariables #-}

module Development.Bake.Server.History(
    History(..), addHistory, readHistory
    ) where

import Development.Bake.Core.Type
import General.Extra
import Control.Concurrent.Extra
import System.IO.Unsafe
import qualified Data.ByteString as BS


data History
    = HQueue -- ^ Promoted by the user, added to the queue
    | HSupersede -- ^ Got superseded by another patch
    | HStart -- ^ Moved into the running queue
    | HPlausible -- ^ Running queue was expanded as its very likely correct
    | HMerge -- ^ Has been merged in
    | HReject -- ^ Got rejected
      deriving (Read,Show)


{-# NOINLINE lock #-}
lock :: Lock
lock = unsafePerformIO newLock


addHistory :: [(History, Patch)] -> IO ()
addHistory [] = return ()
addHistory xs = do
    now <- show <$> getCurrentTime
    withLock lock $ appendFile "history.txt" $ unlines [now ++ " " ++ tail (show h) ++ " " ++ fromPatch p | (h,p) <- xs]

readHistory :: IO BS.ByteString
readHistory = withLock lock $ BS.readFile "history.txt"
