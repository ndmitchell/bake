{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.Pretty(ovenPretty, ovenPrettyMerge, Pretty(..)) where

import Development.Bake.Core.Type
import Data.List.Extra


data Pretty a = Pretty String a deriving (Read,Show,Eq)

instance Stringy a => Stringy (Pretty a) where
    stringyTo (Pretty a b) = a ++ "=" ++ stringyTo b
    stringyFrom s = case breakOn "=" s of
        (a,_:b) -> Pretty a $ stringyFrom b
        _ -> Pretty "" $ stringyFrom s
    stringyPretty (Pretty a b) = a ++ "=" ++ stringyPretty b


ovenPretty :: Oven state patch test -> Oven state (Pretty patch) test
ovenPretty oven@Oven{..} = oven
    {ovenUpdate = \s ps -> ovenUpdate s (map unpretty ps)
    ,ovenPrepare = \s ps -> ovenPrepare s (map unpretty ps)
    ,ovenPatchExtra = \s p -> ovenPatchExtra s (fmap unpretty p)
    ,ovenSupersede = \p1 p2 -> ovenSupersede (unpretty p1) (unpretty p2)
    }
    where
        unpretty :: Pretty a -> a
        unpretty (Pretty _ x) = x

ovenPrettyMerge :: Oven state (Pretty patch) test -> Oven state (Pretty patch) test
ovenPrettyMerge oven = oven
    {ovenSupersede = \(Pretty p1 _) (Pretty p2 _) -> p1 == p2
    }
