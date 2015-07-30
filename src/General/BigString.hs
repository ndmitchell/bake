
module General.BigString(
    TmpFile, newTmpFile, withTmpFile, writeTmpFile, readTmpFile
    ) where

import System.IO.Extra
import Control.DeepSeq
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Concurrent


data TmpFile = TmpFile FilePath (ForeignPtr ())

instance Eq TmpFile where TmpFile a _ == TmpFile b _ = a == b
instance Show TmpFile where show (TmpFile a _) = "TmpFile " ++ show a
instance NFData TmpFile where rnf (TmpFile a b) = rnf a `seq` b `seq` ()

newTmpFile :: IO TmpFile
newTmpFile = do
    (file, close) <- newTempFile
    ptr <- newForeignPtr_ nullPtr
    Foreign.Concurrent.addForeignPtrFinalizer ptr close
    return $ TmpFile file ptr

withTmpFile :: TmpFile -> (FilePath -> IO a) -> IO a
withTmpFile (TmpFile file ptr) op = withForeignPtr ptr $ const $ op file

writeTmpFile :: String -> IO TmpFile
writeTmpFile str = do
    tmp <- newTmpFile
    withTmpFile tmp $ \file -> writeFile file str
    return tmp

readTmpFile :: TmpFile -> IO String
readTmpFile tmp = withTmpFile tmp readFile'
