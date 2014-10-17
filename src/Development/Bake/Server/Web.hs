{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Type
import Development.Bake.Web
import Development.Bake.Util
import Development.Bake.Format
import Data.List.Extra
import Data.Tuple.Extra
import Data.Version
import Paths_bake


web :: Oven State Patch Test -> [(String, String)] -> Server -> IO Output
web Oven{..} args server = return $ OutputHTML $ unlines $
    prefix ++
    (case () of
        _ | Just c <- lookup "client" args ->
                ["<h2>Runs on " ++ c ++ "</h2>"] ++
                runs shower (nostdout server) ((==) (Client c) . qClient)
          | Just t <- lookup "test" args, Just p <- lookup "patch" args ->
                let tt = if t == "" then Nothing else Just $ Test t in
                runs shower server (\Question{..} -> Patch p `elem` snd qCandidate && qTest == tt)
          | Just p <- lookup "patch" args ->
                runs shower server (elem (Patch p) . snd . qCandidate) ++
                ["<h2>Patch information</h2>"] ++
                [e | (pp,(_,e)) <- extra server, Patch p == pp]
          | otherwise ->
                ["<h2>Patches</h2>"] ++
                table "No patches submitted" ["Patch","Status"] (map (patch shower server) patches) ++
                ["<h2>Clients</h2>"] ++
                table "No clients available" ["Name","Running"] (map (client shower server) clients)
    ) ++
    suffix
    where
        patches = submitted server
        clients = sort $ nub $ map (pClient . snd) $ pings server
        shower = Shower
            {showPatch = \p -> tag "a" ["href=?patch=" ++ fromPatch p, "class=patch"] (stringyPretty ovenStringyPatch p)
            ,showTest = \p t -> tag "a" ["href=?patch=" ++ fromPatch p ++ "&" ++ "test=" ++ maybe "" fromTest t] $
                                maybe "Preparing" (stringyPretty ovenStringyTest) t
            }

data Shower = Shower
    {showPatch :: Patch -> String
    ,showTest :: Patch -> Maybe Test -> String
    }


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
    ,".good {font-weight: bold; color: #480}"
    ,".bad {font-weight: bold; color: #800}"
    ,"#footer {margin-top: 40px; font-size: 80%;}"
    ,"</style>"
    ,"</head>"
    ,"<body>"
    ,"<h1>Bake Continuous Integration</h1>"
    ]

suffix =
    ["<p id=footer><a href='https://github.com/ndmitchell/bake'>" ++
        "Copyright Neil Mitchell 2014, version " ++ showVersion version ++ "</a></p>"
    ,"</body>"
    ,"</html>"]

nostdout :: Server -> Server
nostdout s = s{history = [(t,q,fmap (\a -> a{aStdout=""}) a) | (t,q,a) <- history s]}

runs :: Shower -> Server -> (Question -> Bool) -> [String]
runs Shower{..} Server{..} pred = table "No runs" ["Time","Question","Answer"]
    [[show t, show q, show a] | (t,q,a) <- history, pred q]

patch :: Shower -> Server -> (Timestamp, Patch) -> [String]
patch Shower{..} Server{..} (u, p) =
    [showPatch p ++ " by " ++ commasLimit 3 [a | (pp,a) <- authors, Just p == pp] ++ "<br />" ++
     tag "span" ["class=info"] (maybe "" fst (lookup p extra))
    ,if p `elem` concatMap (snd . thd3) updates then tag "span" ["class=good"] "Merged"
     else if p `elem` snd active then
        "Testing (passed " ++ show (length $ filter fst done) ++ " of " ++ (if todo == 0 then "?" else show todo) ++ ")<br />" ++
        tag "span" ["class=info"]
            (if any (not . fst) done then "Retrying " ++ commasLimit 3 [showTest p t | (False,t) <- done]
             else if not $ null running then "Running " ++ commasLimit 3 (map (showTest p) running)
             else "")
     else if p `elem` maybe [] (map snd) paused then "Paused"
     else tag "span" ["class=bad"] "Rejected" ++ "<br />" ++
          tag "span" ["class=info"] (commasLimit 3 [showTest p t | (False,t) <- done, (True,t) `notElem` done])
    ]
    where
        todo = length $ nub
            [ t
            | (_,Question{..},Just Answer{..}) <- history
            , p `elem` snd qCandidate
            , t <- uncurry (++) aTests]
        done = nub
            [ (aSuccess,qTest)
            | (_,Question{..},Just Answer{..}) <- history
            , p `elem` snd qCandidate]
        running = nub
            [ qTest
            | (_,Question{..},Nothing) <- history
            , p `elem` snd qCandidate]

client :: Shower -> Server -> Client -> [String]
client Shower{..} Server{..} c =
    [tag "a" ["href=?client=" ++ fromClient c] $ fromClient c
    ,if null active then "<i>None</i>"
     else commas $ map (uncurry showTest) active]
    where active = [(last $ Patch "" : snd qCandidate, qTest) | (_,Question{..},Nothing) <- history, qClient == c]
