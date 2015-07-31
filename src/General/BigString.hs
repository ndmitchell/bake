
module General.BigString(
    BigString,
    bigStringFromFile, bigStringFromText, bigStringFromString, bigStringFromLazyByteString,
    bigStringToFile, bigStringToText, bigStringToString, bigStringWithString, bigStringToLazyByteString,
    bigStringBackEnd, withBigStringPart
    ) where

import System.IO.Extra
import Control.DeepSeq
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Concurrent
import System.IO.Unsafe
import Control.Exception
import Data.Monoid
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Wai.Parse
import Data.Function
import Control.Monad
import Network.HTTP.Client.MultipartFormData
import Prelude


data BigString = TmpFile FilePath (ForeignPtr ())

instance Monoid BigString where
	mempty = bigStringFromString ""
	mappend a b = bigStringFromString $ bigStringToString a ++ bigStringToString b


instance Show BigString where show x = show (bigStringToText x)
instance NFData BigString where rnf (TmpFile a b) = rnf a `seq` b `seq` ()


bigStringFromFile :: (FilePath -> IO a) -> IO (BigString, a)
bigStringFromFile op = do
	tmp <- newTmpFile
	res <- withTmpFile tmp op
	return (tmp, res)

{-# NOINLINE bigStringFromText #-}
bigStringFromText :: T.Text -> BigString
bigStringFromText x = unsafePerformIO $ do
    tmp <- newTmpFile
    withTmpFile tmp $ \file -> T.writeFile file x
    return tmp

{-# NOINLINE bigStringFromLazyByteString #-}
bigStringFromLazyByteString :: LBS.ByteString -> BigString
bigStringFromLazyByteString x = unsafePerformIO $ do
    tmp <- newTmpFile
    withTmpFile tmp $ \file -> LBS.writeFile file x
    return tmp

{-# NOINLINE bigStringFromString #-}
bigStringFromString :: String -> BigString
bigStringFromString x = unsafePerformIO $ writeTmpFile x

bigStringToFile :: BigString -> FilePath -> IO ()
bigStringToFile x out = withTmpFile x $ \file -> copyFile file out

{-# NOINLINE bigStringToText #-}
bigStringToText :: BigString -> T.Text
bigStringToText x = unsafePerformIO $ withTmpFile x T.readFile

{-# NOINLINE bigStringToString #-}
bigStringToString :: BigString -> String
bigStringToString x = unsafePerformIO $ withTmpFile x readFile'

{-# NOINLINE bigStringWithString #-}
bigStringWithString :: NFData a => BigString -> (String -> a) -> a
bigStringWithString x op = unsafePerformIO $ withTmpFile x $ \file -> do
    src <- readFile file
    let res = op src
    evaluate $ rnf res
    return res

{-# NOINLINE bigStringToLazyByteString #-}
bigStringToLazyByteString :: BigString -> LBS.ByteString
bigStringToLazyByteString x = LBS.fromChunks $ return $ unsafePerformIO $ withTmpFile x BS.readFile

bigStringBackEnd :: BackEnd BigString
bigStringBackEnd _ _ ask = fmap fst $ bigStringFromFile $ \file -> do
    withFile file AppendMode $ \h -> do
        fix $ \loop -> do
            bs <- ask
            unless (BS.null bs) $ do
                BS.hPut h bs
                loop

withBigStringPart :: String -> BigString -> (Part -> IO a) -> IO a
withBigStringPart name body op = withTmpFile body $ \file -> op $ partFileSourceChunked (T.pack name) file


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
