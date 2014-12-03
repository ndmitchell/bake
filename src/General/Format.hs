
module General.Format(
    tag, tagg,
    table,
    escapeHTML
    ) where

import Data.List.Extra


table :: String -> [String] -> [[String]] -> [String]
table zero cols [] = ["<p>" ++ zero ++ "</p>"]
table _ cols body =
    ["<table>"
    ,tagg "thead" $ tagg "tr" $ concatMap (tagg "td") cols
    ,"<tbody>"] ++
    [tagg "tr" $ concatMap (tagg "td") x | x <- body] ++
    ["</tbody>"
    ,"</table>"]


tagg :: String -> String -> String
tagg t = tag t []

tag :: String -> [String] -> String -> String
tag t at x = "<" ++ t ++ concatMap f at ++ ">" ++ x ++ "</" ++ t ++ ">"
    where f x = let (a,b) = break (== '=') x in ' ':a ++ (if null b then "" else "=\"" ++ escapeHTML (drop1 b) ++ "\"")


escapeHTML :: String -> String
escapeHTML = concatMap $ \c -> case c of
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    '&'  -> "&amp;"
    '\"' -> "&quot;"
    '\'' -> "&#39;"
    x    -> [x]
