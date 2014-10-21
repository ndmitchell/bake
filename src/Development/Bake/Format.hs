
module Development.Bake.Format(
    tag, tag_,
    table,
    escapeHTML,
    commas, commasLimit, unwordsLimit
    ) where

import Data.List.Extra


table :: String -> [String] -> [[String]] -> [String]
table zero cols [] = ["<p>" ++ zero ++ "</p>"]
table _ cols body =
    ["<table>"
    ,tag_ "thead" $ tag_ "tr" $ concatMap (tag_ "td") cols
    ,"<tbody>"] ++
    [tag_ "tr" $ concatMap (tag_ "td") x | x <- body] ++
    ["</tbody>"
    ,"</table>"]


tag_ :: String -> String -> String
tag_ t = tag t []

tag :: String -> [String] -> String -> String
tag t at x = "<" ++ t ++ concatMap f at ++ ">" ++ x ++ "</" ++ t ++ ">"
    where f x = let (a,b) = break (== '=') x in ' ':a ++ (if null b then "" else "=\"" ++ escapeHTML (drop1 b) ++ "\"")


commas :: [String] -> String
commas = intercalate ", "

commasLimit :: Int -> [String] -> String
commasLimit i xs = intercalate ", " a ++ (if null b then "" else "...")
    where (a,b) = splitAt i xs

unwordsLimit :: Int -> [String] -> String
unwordsLimit i xs = unwords a ++ (if null b then "" else "...")
    where (a,b) = splitAt i xs


escapeHTML :: String -> String
escapeHTML = concatMap $ \c -> case c of
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    '&'  -> "&amp;"
    '\"' -> "&quot;"
    '\'' -> "&#39;"
    x    -> [x]
