{-# LANGUAGE RecordWildCards #-}

-- | A notion of a string that might take up less space.
module General.Str(
    Str, strInit, strPack, strUnpack,
    strInfo,
    strTest
    ) where

import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Control.Concurrent.Extra
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


data Paged = Paged
    {pagedCount :: !Int -- next index to use
    ,pagedStore :: !(MRU.MRU Int (Int,Text.Text)) -- index, (size,text)
    ,pagedFree :: !Int -- free space in the paged cache
    ,pagedDir :: !FilePath
    ,pagedLock :: !Lock -- Lock to be held while accessing files
    }

{-# NOINLINE paged #-}
paged :: IORef (Maybe Paged)
paged = unsafePerformIO $ newIORef Nothing

strInit :: FilePath -> Int -> IO ()
strInit dir free = do
    createDirectoryIfMissing True dir
    lock <- newLock
    atomicModifyIORef paged $ \Nothing ->
        (Just $ Paged 0 MRU.empty free dir lock, ())

strInfo :: IO String
strInfo = do
    ref <- readIORef paged
    return $ unlines $ case ref of
        Nothing -> ["Not using paged strings"]
        Just Paged{..} ->
            let xs = MRU.toList pagedStore in
            ["Using paged strings in " ++ pagedDir
            ,show pagedCount ++ " paged strings, " ++ show (length xs) ++ " in memory"
            ,show pagedFree ++ " bytes free, " ++ show (sum $ map (fst . snd) xs) ++ " bytes used"
            ]

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
    n <- withLock pagedLock $ withFile (pagedDir </> show i <.> "txt") WriteMode $ \h -> do
        Text.hPutStr h t
        fromIntegral <$> hFileSize h
    return (pagedInsert i (n,t) p{pagedCount = pagedCount+1}, pagedCount)

pagedLookup :: Int -> Paged -> IO (Paged, Text.Text)
pagedLookup i p@Paged{..}
    | Just (_, t) <- MRU.lookup i pagedStore = return (p, t)
    | otherwise = do
        res <- catch_ (
            withLock pagedLock $ withFile (pagedDir </> show i <.> "txt") ReadMode $ \h -> do
                n <- fromIntegral <$> hFileSize h
                t <- Text.hGetContents h
                return (n, t)) $
            \e -> do
                t <- fmap Text.pack $ showException e
                return (Text.length t, t)
        return (pagedInsert i res p, snd res)


data Str = StrPinned Text.Text
         | StrPaged Int

instance Show Str where
    show _ = "Str" -- useful for debugging, and we don't look inside strings
instance Eq Str where
    StrPinned x == StrPinned y = x == y
    StrPaged x == StrPaged y = x == y
    _ == _ = error "Can't do deep equality Eq on Str"
instance NFData Str where
    rnf (StrPinned x) = rnf x
    rnf (StrPaged x) = rnf x


{-# NOINLINE txtPack #-}
txtPack :: Text.Text -> Str
txtPack t | Text.null t = StrPinned Text.empty
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
