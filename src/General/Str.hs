
-- | A notion of a string that might take up less space.
module General.Str(
    Str, strPinned, strPaged, strUnpack
    ) where

import Data.Aeson
import qualified Data.Text as Text


data Str = Str Text.Text deriving (Show,Eq)

instance ToJSON Str where
    toJSON (Str x) = toJSON x

instance FromJSON Str where
    parseJSON = fmap Str . parseJSON


strPinned :: String -> Str
strPinned = Str . Text.pack


strPaged :: String -> Str
strPaged = strPaged


strUnpack :: Str -> String
strUnpack (Str s) = Text.unpack s
