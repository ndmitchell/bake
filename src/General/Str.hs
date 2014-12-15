{-# LANGUAGE RecordWildCards #-}

-- | A notion of a string that might take up less space.
module General.Str(
    Str, strInit, strPack, strUnpack,
    strTest
    ) where

import Control.Monad
import Control.DeepSeq
import Data.Aeson
import Data.IORef
import System.IO.Unsafe
import System.FilePath
import System.IO.Extra
import System.Directory
import Control.Exception.Extra
import Data.Tuple.Extra
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified General.MRU as MRU

fileSize :: FilePath -> IO Int
fileSize file =
    -- open with write privileges, so you touch the modtime for GC
    withFile file AppendMode $ fmap fromIntegral . hFileSize


data Paged = Paged
    {pagedCount :: !Int -- next index to use
    ,pagedStore :: !(MRU.MRU Int (Int,Text.Text)) -- index, (size,text)
    ,pagedFree :: !Int -- free space in the paged cache
    ,pagedDir :: !FilePath
    }

{-# NOINLINE paged #-}
paged :: IORef (Maybe Paged)
paged = unsafePerformIO $ newIORef Nothing

strInit :: FilePath -> Int -> IO ()
strInit dir free = do
    createDirectoryIfMissing True dir
    atomicModifyIORef paged $ \Nothing ->
        (Just $ Paged 0 MRU.empty free dir, ())

pagedEvict :: Paged -> Paged
pagedEvict p@Paged{..}
    | pagedFree < 0, Just (i,rest) <- MRU.delete pagedStore = pagedEvict $ p
        {pagedStore = rest
        ,pagedFree = pagedFree + maybe 0 fst (MRU.lookup i pagedStore)}
    | otherwise = p

pagedInsert :: Int -> (Int,Text.Text) -> Paged -> Paged
pagedInsert i (n,t) p@Paged{..} = pagedEvict $ p
    {pagedStore = MRU.insert i (n,t) pagedStore
    ,pagedFree = pagedFree - n}

pagedAdd :: Text.Text -> Paged -> IO (Paged, Int)
pagedAdd t p@Paged{..} = do
    let i = pagedCount
    let file = pagedDir </> show i <.> "txt"
    Text.writeFile file t
    n <- fileSize file
    return (pagedInsert i (n,t) p{pagedCount = pagedCount+1}, pagedCount)

pagedLookup :: Int -> Paged -> IO (Paged, Text.Text)
pagedLookup i p@Paged{..}
    | Just (_, t) <- MRU.lookup i pagedStore = return (p, t)
    | otherwise = do
        res <- catch_ (do
            let file = pagedDir </> show i <.> "txt"
            t <- Text.readFile file
            n <- fileSize file
            return (n,t)) $
            \e -> do
                t <- fmap Text.pack $ showException e
                return (Text.length t, t)
        return (pagedInsert i res p, snd res)


data Str = StrPinned Text.Text
         | StrPaged Int

instance Show Str where show = strUnpack
instance Eq Str where (==) = error "Can't do Eq on Str"
instance NFData Str where
    rnf (StrPinned x) = rnf x
    rnf (StrPaged x) = rnf x


{-# NOINLINE txtPack #-}
txtPack :: Text.Text -> Str
txtPack t = unsafePerformIO $
    atomicModifyIORef paged $ \p -> case p of
        Nothing -> (Nothing, StrPinned t)
        Just p -> (Just *** StrPaged) $ unsafePerformIO $ pagedAdd t p

txtUnpack :: Str -> Text.Text
txtUnpack (StrPinned t) = t
txtUnpack (StrPaged i) = unsafePerformIO $ atomicModifyIORef paged $
    \(Just p) -> first Just $ unsafePerformIO $ pagedLookup i p


instance ToJSON Str where
    toJSON = toJSON . txtUnpack

instance FromJSON Str where
    parseJSON = fmap txtPack . parseJSON

strPack :: String -> Str
strPack = txtPack . Text.pack

strUnpack :: Str -> String
strUnpack = Text.unpack . txtUnpack


strTest :: IO ()
strTest = withTempDir $ \dir ->
    bracket_ (strInit dir 100) (writeIORef paged Nothing) $ do
        putStrLn "Testing Str paging"
        let f i = map show [1..i]
        let xs = map strPack $ f 10
        evaluate $ rnf xs
        when (map strUnpack xs /= f 10) $ error "Str mismatch"
        let xs = map strPack $ f 1000
        evaluate $ rnf xs
        when (map strUnpack xs /= f 1000) $ error "Str mismatch"
