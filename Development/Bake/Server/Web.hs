{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Type
import Development.Bake.Web
import Data.List

web :: Input -> Server -> IO Output
web _ server = return $ OutputHTML $ unlines $
    prefix ++
    concatMap (status server) (submitted server) ++
    suffix

prefix =
    ["<!HTML>"
    ,"<html>"
    ,"<head>"
    ,"</head>"
    ,"<body>"
    ,"<table border=1>"]

suffix =
    ["</table>"
    ,"</body>"
    ,"</html>"]

status :: Server -> Patch -> [String]
status Server{..} p =
    ["<tr>"
    ,"<td>"
    ,"Patch " ++ show p ++ " from " ++ intercalate ", " [a | (pp,a) <- authors, Just p == pp]
    ,"</td>"
    ,"<td>"
    ,if p `elem` concatMap (candidatePatches . snd) updates then "In the main repo"
     else if p `elem` candidatePatches active then
        "Actively being worked on " ++ show (length $ filter fst done) ++
        " out of " ++ show todo ++ " (" ++ show (length $ filter (not . fst) done) ++ " failing)"
     else "Rejected"
    ,"</td>"
    ,"</tr>"
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
