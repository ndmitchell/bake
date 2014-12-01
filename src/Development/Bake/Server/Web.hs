{-# LANGUAGE RecordWildCards, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Core.Type
import General.Web
import General.Extra
import General.Format
import General.DelayCache
import General.Str
import Data.List.Extra
import Data.Tuple.Extra
import System.Time.Extra
import Data.Version
import Paths_bake


web :: Oven State Patch Test -> [(String, String)] -> Server -> IO Output
web oven@Oven{..} args server = do
    extra <- askDelayCache $ extra server
    shower <- shower extra oven
    return $ OutputHTML $ unlines $
        prefix ++
        (if null args then
            ["<h1>Bake Continuous Integration</h1>"
            ,"<h2>Patches</h2>"] ++
            table "No patches submitted" ["Patch","Time","Status"] (map (patch shower server) $ linearise server) ++
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
                [p] | null $ ask "test", Just (_, e) <- extra $ Patch p ->
                        ["<h2>Patch information</h2>", strUnpack e]
                _ -> [])
        ) ++
        suffix
    where
        clients = sort $ nub $ map (pClient . snd) $ pings server


linearise :: Server -> [Either State Patch]
linearise Server{..} = reverse $ map snd $ sortOn fst $ states ++ patches
    where
        states = [ (fmap fst3 $ find ((==) s . snd3) updates, Left s)
                 | s <- nub $ fst active : map snd3 updates ++ map (fst . thd3) updates]
        patches = map (Just *** Right) submitted


data Shower = Shower
    {showPatch :: Patch -> String
    ,showPatchExtra :: Patch -> String
    ,showTest :: Maybe Test -> String
    ,showTestPatch :: Patch -> Maybe Test -> String
    ,showTestQuestion :: Question -> String
    ,showState :: State -> String
    ,showTime :: Timestamp -> String
    }

showThreads i = show i ++ " thread" ++ ['s' | i /= 1]

shower :: (Patch -> Maybe (Str, Str)) -> Oven State Patch Test -> IO Shower
shower extra Oven{..} = do
    showTime <- showRelativeTimestamp
    return $ Shower
        {showPatch = \p -> tag "a" ["href=?patch=" ++ fromPatch p, "class=patch"] (stringyPretty ovenStringyPatch p)
        ,showPatchExtra = \p -> maybe "" (strUnpack . fst) $ extra p
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
    ,".good {font-weight: bold; color: darkgreen;}"
    ,".bad {font-weight: bold; color: darkred;}"
    ,".nobr {white-space: nowrap;}"
    ,".red {color: darkred;}"
    ,".green {color: darkgreen;}"
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
        [(_,_,Just Answer{..})] -> ["<pre>"] ++ lines (escapeHTML $ strUnpack aStdout) ++ ["</pre>"]
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


patch :: Shower -> Server -> Either State Patch -> [String]
patch Shower{..} Server{..} (Left s) =
    [showState s
    ,maybe "" (showTime . fst3) $ find ((==) s . snd3) updates
    ,if s /= fst active || null running then tag "span" ["class=good"] "Valid"
     else "Testing (passed " ++ show (length $ nubOn (qTest . snd) $ filter fst done) ++ " of " ++ (if todo == 0 then "?" else show (todo+1)) ++ ")<br />" ++
        tag "span" ["class=info"]
            (if any (not . fst) done then "Retrying " ++ commasLimit 3 (nub [showTest (qTest t) | (False,t) <- done])
             else "Running " ++ commasLimit 3 (map showTestQuestion running))
    ]
    where
        todo = length $ nub
            [ t
            | (_,Question{..},Just Answer{..}) <- history
            , (s, []) == qCandidate
            , t <- uncurry (++) aTests]
        done = nub
            [ (aSuccess,q)
            | (_,q@Question{..},Just Answer{..}) <- history
            , (s, []) == qCandidate]
        running = nub
            [ q
            | (_,q@Question{..},Nothing) <- history
            , (s, []) == qCandidate]


patch Shower{..} Server{..} (Right p) =
    [showPatch p ++ " by " ++ commasLimit 3 [a | (pp,a) <- authors, Just p == pp] ++ "<br />" ++
     tag "span" ["class=info"] (showPatchExtra p)
    ,maybe "" (showTime . fst) $ find ((==) p . snd) submitted
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
