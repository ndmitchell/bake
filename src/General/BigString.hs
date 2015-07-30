
module General.BigString(
    BigString, newTmpFile, withTmpFile, writeTmpFile, readTmpFile
    ) where

import System.IO.Extra
import Control.DeepSeq
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Concurrent


data BigString = TmpFile FilePath (ForeignPtr ())

instance Eq BigString where TmpFile a _ == TmpFile b _ = a == b
instance Show BigString where show (TmpFile a _) = "TmpFile " ++ show a
instance NFData BigString where rnf (TmpFile a b) = rnf a `seq` b `seq` ()

newTmpFile :: IO BigString
newTmpFile = do
    (file, close) <- newTempFile
    ptr <- newForeignPtr_ nullPtr
    Foreign.Concurrent.addForeignPtrFinalizer ptr close
    return $ TmpFile file ptr

withTmpFile :: BigString -> (FilePath -> IO a) -> IO a
withTmpFile (TmpFile file ptr) op = withForeignPtr ptr $ const $ op file

writeTmpFile :: String -> IO BigString
writeTmpFile str = do
    tmp <- newTmpFile
    withTmpFile tmp $ \file -> writeFile file str
    return tmp

readTmpFile :: BigString -> IO String
readTmpFile tmp = withTmpFile tmp readFile'
