{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Type
import Development.Bake.Web
import Data.List.Extra
import Data.Time.Clock
import Data.Tuple.Extra


web :: Oven State Patch Test -> Input -> Server -> IO Output
web oven _ server = return $ OutputHTML $ unlines $
    prefix ++
    ["<h2>Patches</h2>"] ++
    table "No patches submitted" ["Patch","Status"] (map (patch oven server) patches) ++
    ["<h2>Clients</h2>"] ++
    table "No clients available" ["Name","Running"] (map (client oven server) clients) ++
    suffix
    where
        patches = submitted server
        clients = sort $ nub $ map (pClient . snd) $ pings server

table :: String -> [String] -> [[String]] -> [String]
table zero cols [] = ["<p>" ++ zero ++ "</p>"]
table _ cols body =
    ["<table>"
    ,tag_ "thead" $ tag_ "tr" $ concatMap (tag_ "td") cols
    ,"<tbody>"] ++
    [tag_ "tr" $ concatMap (tag_ "td") x | x <- body] ++
    ["</tbody>"
    ,"</table>"]

prefix =
    ["<!HTML>"
    ,"<html>"
    ,"<head>"
    ,"<title>Bake Continuous Integration</title>"
    ,"<style type='text/css'>"
    ,"body, td {font-family: sans-serif; font-size: 10pt;}"
    ,"table {border-collapse: collapse;}"
    ,"table, td {border: 1px solid #ccc;}"
    ,"td {padding: 2px; padding-right: 15px;}"
    ,"thead {font-weight: bold;}"
    ,"a {text-decoration: none; color: #4183c4;}"
    ,"a:hover {text-decoration: underline;}"
    ,".patch {font-family: Consolas, monospace;}"
    ,".info {font-size: 75%; color: #888;}"
    ,"a.info {color: #4183c4;}" -- tie breaker
    ,".merged {font-weight: bold; color: #480}"
    ,".rejected {font-weight: bold; color: #800}"
    ,"#footer {margin-top: 40px; font-size: 80%;}"
    ,"</style>"
    ,"</head>"
    ,"<body>"
    ,"<h1>Bake Continuous Integration</h1>"
    ]

suffix =
    ["<p id=footer><a href='https://github.com/ndmitchell/bake'>Copyright Neil Mitchell 2014</a></p>"
    ,"</body>"
    ,"</html>"]

tag_ :: String -> String -> String
tag_ t = tag t []

tag :: String -> [String] -> String -> String
tag t at x = "<" ++ t ++ concatMap f at ++ ">" ++ x ++ "</" ++ t ++ ">"
    where f x = let (a,b) = break (== '=') x in ' ':a ++ (if null b then "" else "=\"" ++ drop1 b ++ "\"")

patch :: Oven State Patch Test -> Server -> (UTCTime, Patch) -> [String]
patch Oven{..} Server{..} (u, p) =
    [tag "a" ["href=?patch=" ++ fromPatch p, "class=patch"] (stringyPretty ovenStringyPatch p) ++
     " by " ++ intercalate ", " [a | (pp,a) <- authors, Just p == pp] ++ "<br/>" ++
     tag "span" ["class=info"] (maybe "" fst (lookup p extra))
    ,if p `elem` concatMap (candidatePatches . thd3) updates then tag "span" ["class=merged"] "Merged"
     else if p `elem` candidatePatches active then
        "Actively being worked on " ++ show (length $ filter fst done) ++
        " out of " ++ show todo ++ " (" ++ show (length $ filter (not . fst) done) ++ " failing)"
     else if p `elem` maybe [] (map snd) paused then "Paused"
     else tag "span" ["class=rejected"] "Rejected" ++ "<br />" ++
          tag "span" ["class=info"] (intercalate ", " [maybe "Preparing" (stringyPretty ovenStringyTest) t | (False,t) <- done, (True,t) `notElem` done])
    ]
    where
        todo = length $ nub
            [ t
            | (_,Question{..},Just Answer{..}) <- history
            , p `elem` candidatePatches qCandidate
            , t <- uncurry (++) aTests]
        done = nub
            [ (aSuccess,qTest)
            | (_,Question{..},Just Answer{..}) <- history
            , p `elem` candidatePatches qCandidate]

client :: Oven State Patch Test -> Server -> Client -> [String]
client Oven{..} Server{..} c =
    [tag "a" ["href=?client=" ++ fromClient c] $ fromClient c
    ,if null active then "<i>None</i>"
     else intercalate ", " $ map (maybe "Preparing" (stringyPretty ovenStringyTest)) active]
    where active = [qTest | (_,Question{..},Nothing) <- history, qClient == c]
