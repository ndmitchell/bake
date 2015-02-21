{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module General.Equal(
    Equal, fromEqual, newEqual
    ) where

import Data.IORef
import Data.Unique
import System.IO.Unsafe
import Data.Hashable


data Equal a = Equal (IORef Unique) Int a

instance Show a => Show (Equal a) where
    show = show . fromEqual

fromEqual :: Equal a -> a
fromEqual (Equal _ _ x) = x

newEqual :: Hashable a => a -> Equal a
newEqual x = unsafePerformIO $ do u <- newIORef =<< newUnique; return $ Equal u (hash x) x

instance Eq a => Eq (Equal a) where
    Equal u1 h1 x1 == Equal u2 h2 x2
        | h1 /= h2 = False
        | otherwise = unsafePerformIO $ do
            uu1 <- readIORef u1
            uu2 <- readIORef u2
            if uu1 == uu2 then return True
             else if x1 /= x2 then return False
             else do
                if uu1 > uu2 then writeIORef u1 uu2 else writeIORef u2 uu1
                return True

instance Ord a => Ord (Equal a) where
    compare (Equal u1 h1 x1) (Equal u2 h2 x2)
        | h1 /= h2 = compare h1 h2
        | otherwise = unsafePerformIO $ do
            uu1 <- readIORef u1
            uu2 <- readIORef u2
            case compare x1 x2 of
                _ | uu1 == uu2 -> return EQ
                EQ -> do if uu1 > uu2 then writeIORef u1 uu2 else writeIORef u2 uu1; return EQ
                x -> return x
