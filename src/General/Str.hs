{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A notion of a string that might take up less space.
module General.Str(
    Str, strPack, strUnpack
    ) where

import Control.DeepSeq
import Data.Aeson
import qualified Data.Text as Text


newtype Str = Str Text.Text deriving (Show,Eq,NFData)

instance ToJSON Str where
    toJSON (Str x) = toJSON x

instance FromJSON Str where
    parseJSON = fmap Str . parseJSON


strPack :: String -> Str
strPack = Str . Text.pack


strUnpack :: Str -> String
strUnpack (Str s) = Text.unpack s
