{-# LANGUAGE ScopedTypeVariables #-}

module Development.Bake.Util(
    sleep, duration,
    withTempFile, withTempDir,
    withCurrentDirectory, withTempDirCurrent,
    (&&^), whenJust,
    showException,
    fst3, snd3, thd3,
    unit,
    try_, handle_,
    trim, ltrim, rtrim
    ) where

import qualified System.IO.Temp as T
import Control.Concurrent
import Control.Exception
import System.Directory
import System.IO
import Data.Time.Clock
import Data.Char


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000


offsetTime :: IO (IO Double)
offsetTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ diffTime end start

diffTime :: UTCTime -> UTCTime -> Double
diffTime end start = fromRational $ toRational $ end `diffUTCTime` start

duration :: IO a -> IO (Double, a)
duration act = do
    time <- offsetTime
    res <- act
    time <- time
    return (time, res)


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

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

showException :: SomeException -> IO String
showException = f . show
    where
        f xs = do
            r <- try $ evaluate xs
            case r of
                Left (e :: SomeException) -> return "<NestedException>"
                Right [] -> return []
                Right (x:xs) -> fmap (x :) $ f xs

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

unit :: m () -> m ()
unit = id


try_ :: IO a -> IO (Either SomeException a)
try_ = try

handle_ :: (SomeException -> IO a) -> IO a -> IO a
handle_ = handle

trim, ltrim, rtrim :: String -> String
trim = ltrim . rtrim
ltrim = dropWhile isSpace
rtrim = reverse . ltrim . reverse

