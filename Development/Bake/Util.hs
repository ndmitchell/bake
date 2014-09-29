
module Development.Bake.Util(
    sleep, timed,
    newCookie,
    withTempFile, withTempDir,
    withCurrentDirectory, withTempDirCurrent,
    (&&^)
    ) where

import qualified System.IO.Temp as T
import Control.Concurrent
import Control.Exception
import System.Directory
import System.IO


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000


newCookie :: IO String
newCookie = error "newCookie"


timed :: IO a -> IO (Double, a)
timed = error "timed"


withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile template act = T.withSystemTempFile template $ \file h -> hClose h >> act file

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = T.withSystemTempDirectory "bake"

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir act =
    bracket getCurrentDirectory setCurrentDirectory $ const $ do
        setCurrentDirectory dir; act

withTempDirCurrent :: IO a -> IO a
withTempDirCurrent act = withTempDir $ \t -> withCurrentDirectory t act

(&&^) :: Monad m => m Bool -> m Bool -> m Bool
(&&^) a b = do a <- a; if a then b else return False
