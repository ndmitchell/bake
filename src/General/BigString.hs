
module General.BigString(
    BigString, bigStringFromFile, bigStringFromText, bigStringFromString, bigStringFromLazyByteString,
    newTmpFile, withTmpFile, writeTmpFile, readTmpFile
    ) where

import System.IO.Extra
import Control.DeepSeq
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Concurrent
import System.IO.Unsafe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LBS
import Prelude


data BigString = TmpFile FilePath (ForeignPtr ())

instance Monoid BigString where
	mempty = bigStringFromString ""
	mappend a b = unsafePerformIO $ do
		a <- readTmpFile a
		b <- readTmpFile b
		writeTmpFile $ a ++ b


instance Eq BigString where TmpFile a _ == TmpFile b _ = a == b
instance Show BigString where show (TmpFile a _) = "TmpFile " ++ show a
instance NFData BigString where rnf (TmpFile a b) = rnf a `seq` b `seq` ()


bigStringFromFile :: (FilePath -> IO a) -> IO (BigString, a)
bigStringFromFile = undefined

bigStringFromText :: T.Text -> BigString
bigStringFromText x = unsafePerformIO $ do
    tmp <- newTmpFile
    withTmpFile tmp $ \file -> T.writeFile file x
    return tmp

bigStringFromLazyByteString :: LBS.ByteString -> BigString
bigStringFromLazyByteString x = unsafePerformIO $ do
    tmp <- newTmpFile
    withTmpFile tmp $ \file -> LBS.writeFile file x
    return tmp

bigStringFromString :: String -> BigString
bigStringFromString x = unsafePerformIO $ writeTmpFile x


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
