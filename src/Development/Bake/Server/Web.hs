{-# LANGUAGE RecordWildCards, ViewPatterns #-}

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
web oven@Oven{..} args server = do
    shower <- shower oven
    return $ OutputHTML $ unlines $
        prefix ++
        (if null args then
            ["<h1>Bake Continuous Integration</h1>"
            ,"<h2>Patches</h2>"] ++
            table "No patches submitted" ["Patch","Status"] (map (patch shower server) patches) ++
            ["<h2>Clients</h2>"] ++
            table "No clients available" ["Name","Running"] (map (client shower server) clients)
         else
            let ask x = map snd $ filter ((==) x . fst) args in
            ["<h1><a href='?'>Bake Continuous Integration</a></h1>"] ++
            runs shower server (\Question{..} ->
                let or0 xs = if null xs then True else or xs in
                or0 [qClient == Client c | c <- ask "client"] &&
                or0 [qTest == if t == "" then Nothing else Just (Test t) | t <- ask "test"] &&
                case ask "state" of
                    [] -> or0 [Patch p `elem` snd qCandidate | p <- ask "patch"]
                    s:_ -> qCandidate == (State s, map Patch $ ask "patch")) ++
            (case ask "patch" of
                [p] -> ["<h2>Patch information</h2>"] ++
                       [e | (pp,(_,e)) <- extra server, Patch p == pp]
                _ -> [])
        ) ++
        suffix
    where
        patches = submitted server
        clients = sort $ nub $ map (pClient . snd) $ pings server


data Shower = Shower
    {showPatch :: Patch -> String
    ,showTest :: Maybe Test -> String
    ,showTestPatch :: Patch -> Maybe Test -> String
    ,showTestQuestion :: Question -> String
    ,showState :: State -> String
    ,showTime :: Timestamp -> String
    }

showThreads i = show i ++ " thread" ++ ['s' | i /= 1]
showDuration (ceiling -> i) = show i ++ "s"

shower :: Oven State Patch Test -> IO Shower
shower Oven{..} = do
    showTime <- showRelativeTimestamp
    return $ Shower
        {showPatch = \p -> tag "a" ["href=?patch=" ++ fromPatch p, "class=patch"] (stringyPretty ovenStringyPatch p)
        ,showState = \s -> tag "a" ["href=?state=" ++ fromState s, "class=state"] (stringyPretty ovenStringyState s)
        ,showTest = f Nothing Nothing []
        ,showTestPatch = \p -> f Nothing Nothing [p]
        ,showTestQuestion = \Question{..} -> f (Just qClient) (Just $ fst qCandidate) (snd qCandidate) qTest
        ,showTime = showTime
        }
    where
        f c s ps t =
            tag "a" ["href=?" ++ intercalate "&" parts] $
            maybe "Preparing" (stringyPretty ovenStringyTest) t
            where parts = ["client=" ++ fromClient c | Just c <- [c]] ++
                          ["state=" ++ fromState s | Just s <- [s]] ++
                          ["patch=" ++ fromPatch p | p <- ps] ++
                          ["test=" ++ maybe "" fromTest t]

prefix =
    ["<!HTML>"
    ,"<html>"
    ,"<head>"
    ,"<title>Bake Continuous Integration</title>"
    ,"<link rel='shortcut icon' type='image/x-icon' href='html/favicon.ico' />"
    ,"<style type='text/css'>"
    ,"body, td {font-family: sans-serif; font-size: 10pt;}"
    ,"table {border-collapse: collapse;}"
    ,"table, td {border: 1px solid #ccc;}"
    ,"td {padding: 2px; padding-right: 15px;}"
    ,"thead {font-weight: bold;}"
    ,"a {text-decoration: none; color: #4183c4;}"
    ,"a:hover {text-decoration: underline;}"
    ,".patch, .state {font-family: Consolas, monospace; white-space:nowrap;}"
    ,".info {font-size: 75%; color: #888;}"
    ,"a.info {color: #4183c4;}" -- tie breaker
    ,".good {font-weight: bold; color: #480}"
    ,".bad {font-weight: bold; color: #800}"
    ,".nobr {white-space: nowrap;}"
    ,"#footer {margin-top: 40px; font-size: 80%;}"
    ,"</style>"
    ,"</head>"
    ,"<body>"
    ]

suffix =
    ["<p id=footer><a href='https://github.com/ndmitchell/bake'>" ++
        "Copyright Neil Mitchell 2014, version " ++ showVersion version ++ "</a></p>"
    ,"</body>"
    ,"</html>"]

runs :: Shower -> Server -> (Question -> Bool) -> [String]
runs Shower{..} Server{..} pred = table "No runs" ["Time","Question","Answer"]
    [[tag "span" ["class=nobr"] $ showTime t, showQuestion q, showAnswer a] | (t,q,a) <- good] ++
    (case good of
        [(_,_,Just Answer{..})] -> ["<pre>"] ++ lines aStdout ++ ["</pre>"]
        _ -> [])
    where
        good = filter (pred . snd3) history
        showQuestion q@Question{..} =
            "With " ++ showState (fst qCandidate) ++
            (if null $ snd qCandidate then "" else " plus ") ++
            commas (map showPatch $ snd qCandidate) ++ "<br />" ++
            "Test " ++ showTestQuestion q ++ " on " ++
            fromClient qClient ++ " with " ++ showThreads qThreads
        showAnswer Nothing = "<i>Running...</i>"
        showAnswer (Just Answer{..}) =
            if aSuccess then tag "span" ["class=good"] ("Succeeded in " ++ showDuration aDuration)
                        else tag "span" ["class=bad"]  ("Failed in "    ++ showDuration aDuration)


patch :: Shower -> Server -> (Timestamp, Patch) -> [String]
patch Shower{..} Server{..} (u, p) =
    [showPatch p ++ " by " ++ commasLimit 3 [a | (pp,a) <- authors, Just p == pp] ++ "<br />" ++
     tag "span" ["class=info"] (maybe "" fst (lookup p extra))
    ,if p `elem` concatMap (snd . thd3) updates then tag "span" ["class=good"] "Merged"
     else if p `elem` snd active then
        "Testing (passed " ++ show (length $ nubOn (qTest . snd) $ filter fst done) ++ " of " ++ (if todo == 0 then "?" else show (todo+1)) ++ ")<br />" ++
        tag "span" ["class=info"]
            (if any (not . fst) done then "Retrying " ++ commasLimit 3 (nub [showTestPatch p (qTest t) | (False,t) <- done])
             else if not $ null running then "Running " ++ commasLimit 3 (map showTestQuestion running)
             else "")
     else if p `elem` maybe [] (map snd) paused then "Paused"
     else tag "span" ["class=bad"] "Rejected" ++ "<br />" ++
          tag "span" ["class=info"] (commasLimit 3 [showTestQuestion q | (False,q) <- done, [p] `isSuffixOf` snd (qCandidate q)])
    ]
    where
        todo = length $ nub
            [ t
            | (_,Question{..},Just Answer{..}) <- history
            , p `elem` snd qCandidate
            , t <- uncurry (++) aTests]
        done = nub
            [ (aSuccess,q)
            | (_,q@Question{..},Just Answer{..}) <- history
            , p `elem` snd qCandidate]
        running = nub
            [ q
            | (_,q@Question{..},Nothing) <- history
            , p `elem` snd qCandidate]

client :: Shower -> Server -> Client -> [String]
client Shower{..} Server{..} c =
    [tag "a" ["href=?client=" ++ fromClient c] $ fromClient c
    ,if null active then "<i>None</i>"
     else commas $ map showTestQuestion active]
    where active = [q | (_,q@Question{..},Nothing) <- history, qClient == c]
