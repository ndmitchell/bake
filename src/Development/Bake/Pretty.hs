{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.Pretty(ovenPretty, ovenPrettyMerge, Pretty(..)) where

import Development.Bake.Core.Type
import Data.List.Extra


data Pretty a = Pretty String a deriving (Read,Show,Eq)

prettyStringy :: String -> Stringy a -> Stringy (Pretty a)
prettyStringy sep Stringy{..} = Stringy
    {stringyTo = \(Pretty a b) -> a ++ sep ++ stringyTo b
    ,stringyFrom = \s -> let (a,b) = breakOn sep s in
        if null b then Pretty "" $ stringyFrom a else Pretty a $ stringyFrom $ drop (length sep) b
    ,stringyPretty = \(Pretty a b) -> a ++ sep ++ stringyPretty b
    }

ovenPretty :: String -> Oven state patch test -> Oven state (Pretty patch) test
ovenPretty sep oven@Oven{..} = oven
    {ovenUpdate = \s ps -> ovenUpdate s (map unpretty ps)
    ,ovenPrepare = \s ps -> ovenPrepare s (map unpretty ps)
    ,ovenPatchExtra = \s p -> ovenPatchExtra s (fmap unpretty p)
    ,ovenSupersede = \p1 p2 -> ovenSupersede (unpretty p1) (unpretty p2)
    ,ovenStringyPatch = prettyStringy sep ovenStringyPatch
    }
    where
        unpretty :: Pretty a -> a
        unpretty (Pretty _ x) = x

ovenPrettyMerge :: Oven state (Pretty patch) test -> Oven state (Pretty patch) test
ovenPrettyMerge oven = oven
    {ovenSupersede = \(Pretty p1 _) (Pretty p2 _) -> p1 == p2
    }
