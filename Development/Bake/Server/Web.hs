{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Type
import Development.Bake.Web
import Data.List
import Data.Time.Clock
import Data.Tuple.Extra


web :: Oven State Patch Test -> Input -> Server -> IO Output
web oven _ server = return $ OutputHTML $ unlines $
    prefix ++
    ["<h2>Patches</h2>"] ++
    table "No patches submitted" ["Patch","Status"] (map (patch oven server) patches) ++
    ["<h2>Clients</h2>"] ++
    table "No clients available" ["Name"] (map (client server) clients) ++
    suffix
    where
        patches = submitted server
        clients = nub $ map (pClient . snd) $ pings server

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
    ,"</head>"
    ,"<body>"
    ,"<h1>Bake Continuous Integration</h1>"
    ]

suffix =
    ["<p><a href='https://github.com/ndmitchell/bake'>Copyright Neil Mitchell 2014</a></p>"
    ,"</body>"
    ,"</html>"]

tag_ :: String -> String -> String
tag_ t x = "<" ++ t ++ ">" ++ x ++ "</" ++ t ++ ">"

patch :: Oven State Patch Test -> Server -> (UTCTime, Patch) -> [String]
patch Oven{..} Server{..} (u, p) =
    ["Patch " ++ stringyPretty ovenStringyPatch p ++ " from " ++ intercalate ", " [a | (pp,a) <- authors, Just p == pp]
    ,if p `elem` concatMap (candidatePatches . thd3) updates then "In the main repo"
     else if p `elem` candidatePatches active then
        "Actively being worked on " ++ show (length $ filter fst done) ++
        " out of " ++ show todo ++ " (" ++ show (length $ filter (not . fst) done) ++ " failing)"
     else "Rejected"
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

client :: Server -> Client -> [String]
client Server{..} c = [show c]
