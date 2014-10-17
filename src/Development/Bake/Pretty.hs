{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Development.Bake.Pretty(ovenPretty, Pretty(..)) where

import Development.Bake.Type
import Data.List.Extra
import Control.Arrow
import Debug.Trace


data Pretty a = Pretty String a deriving (Read,Show,Eq)

prettyStringy :: Show a => String -> Stringy a -> Stringy (Pretty a)
prettyStringy sep Stringy{..} = Stringy
    {stringyTo = \(Pretty a b) -> a ++ sep ++ stringyTo b
    ,stringyFrom = \s -> let (a,b) = breakOn sep s in
        trace (show (s, if null b then Pretty "" $ stringyFrom a else Pretty a $ stringyFrom $ drop (length sep) b)) $
        if null b then Pretty "" $ stringyFrom a else Pretty a $ stringyFrom $ drop (length sep) b
    ,stringyPretty = \(Pretty a b) -> a ++ sep ++ stringyPretty b
    }

ovenPretty :: Show patch => String -> Oven state patch test -> Oven state (Pretty patch) test
ovenPretty sep oven@Oven{..} = oven
    {ovenUpdateState = ovenUpdateState . fmap (second $ map unpretty)
    ,ovenPrepare = \s ps -> ovenPrepare s (map unpretty ps)
    ,ovenPatchExtra = \s p -> ovenPatchExtra s (fmap unpretty p)
    ,ovenStringyPatch = prettyStringy sep ovenStringyPatch
    }
    where
        unpretty :: Pretty a -> a
        unpretty (Pretty _ x) = x
