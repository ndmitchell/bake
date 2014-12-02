{-# LANGUAGE RecordWildCards, ViewPatterns #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Web
import General.Extra
import General.Format
import General.DelayCache
import General.Str
import Data.List.Extra
import Data.Tuple.Extra
import System.Time.Extra
import Data.Version
import Data.Maybe
import Paths_bake
import Development.Bake.Server.Query
import qualified Data.Map as Map


web :: Oven State Patch Test -> [(String, String)] -> Server -> IO Output
web oven@Oven{..} args server = do
    extra <- askDelayCache $ extra server
    shower <- shower extra oven
    return $ OutputHTML $ unlines $
        prefix ++
        (if null args then
            ["<h1>Bake Continuous Integration</h1>"
            ,"<h2>Patches</h2>"] ++
            failures shower server ++
            table "No patches submitted" ["Patch","Time","Status"]
                (map (patch shower server) $ nub (map (Just . snd) $ submitted server) ++ [Nothing]) ++
            ["<h2>Clients</h2>"] ++
            table "No clients available" ["Name","Running"]
                (map (client shower server) $ Map.keys $ pings server)
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
                [p] | null $ ask "test", Just (_, e) <- extra $ Right $ Patch p ->
                        ["<h2>Patch information</h2>", strUnpack e]
                _ -> [])
        ) ++
        suffix


data Shower = Shower
    {showPatch :: Patch -> String
    ,showExtra :: Either State Patch -> String
    ,showTest :: Maybe Test -> String
    ,showTestPatch :: Patch -> Maybe Test -> String
    ,showTestQuestion :: Question -> String
    ,showState :: State -> String
    ,showTime :: Timestamp -> String
    }

showThreads i = show i ++ " thread" ++ ['s' | i /= 1]

shower :: (Either State Patch -> Maybe (Str, Str)) -> Oven State Patch Test -> IO Shower
shower extra Oven{..} = do
    showTime <- showRelativeTimestamp
    return $ Shower
        {showPatch = \p -> tag "a" ["href=?patch=" ++ fromPatch p, "class=patch"] (stringyPretty ovenStringyPatch p)
        ,showExtra = \e -> maybe "" (strUnpack . fst) $ extra e
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


failures :: Shower -> Server -> [String]
failures Shower{..} server
    | null xs = []
    | otherwise = [tag_ "p" "Tracking down failures in:", tag_ "ul" $ concatMap (tag_ "li" . showTest) xs]
    where xs = nub $ map fst $ targetFailures server


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


patch :: Shower -> Server -> Maybe Patch -> [String]
patch Shower{..} server@Server{..} p =
    [maybe ("Initial state " ++ showState s0) showPatch p ++
     " by " ++ commasLimit 3 (Map.findWithDefault [] p authors) ++ "<br />" ++
     tag "span" ["class=info"] (showExtra $ maybe (Left s0) Right p)

    ,maybe "" (showTime . fst) $ find ((==) p . Just . snd) submitted

    ,case patchStatus server p of
        Accepted -> tag "span" ["class=good"] "Merged"
        Unknown -> "Testing (passed 0 of ?)" ++ running
        Paused -> "Paused"
        Rejected xs -> tag "span" ["class=bad"] "Rejected" ++ "<br />" ++
            tag "span" ["class=info"] (commasLimit 3 $ map showTestQuestion xs)
        Progressing done todo ->
            "Testing (passed " ++ show (length done + 1) ++ " of " ++ show (length (done++todo) + 1) ++ ")" ++ running
    ]
    where
        s0 = state0 server
        running | null xs = ""
                | otherwise = "<br />" ++ tag "span" ["class=info"] (commasLimit 3 items)
            where xs = unanswered server [maybe (candidate' (s0,[])) patch' p]
                  (yes,no) = partition (maybe null (isSuffixOf . return) p . snd . qCandidate) xs
                  items = map (tag_ "b" . showTestQuestion) yes ++ map showTestQuestion no


client :: Shower -> Server -> Client -> [String]
client Shower{..} server c =
    [tag "a" ["href=?client=" ++ fromClient c] $ fromClient c
    ,if null target then "<i>None</i>"
     else commas $ map showTestQuestion target]
    where target = unanswered server [client' c]


---------------------------------------------------------------------
-- LOGIC

data Status
    = Unknown                    -- ^ Been given, not yet been prepared
    | Progressing [Test] [Test]  -- ^ In progress, on left have been done, on right still todo.
                                 --   Once all done, not yet accepted, merely plausible.
    | Accepted                   -- ^ Accepted, rolled into production
    | Paused                     -- ^ In the pause queue
    | Rejected [Question]        -- ^ Rejected, because of the following tests


-- | Nothing stands for using the zero patch on the initial state 
patchStatus :: Server -> Maybe Patch -> Status
-- Simple cases
patchStatus server (Just p)
    | p `elem` concatMap (snd . thd3) (updates server) = Accepted
    | p `elem` maybe [] (map snd) (paused server) = Paused
patchStatus server Nothing
    | not $ null $ updates server = Accepted

-- Detect rejection
patchStatus server (Just p)
    -- we may have previously failed, but been requeued, so if we're active don't hunt for reject
    | p `notElem` snd (target server)
    , bad <- answered server [lastPatch' p, blame']
    , not $ null bad
    = Rejected $ nub $ map fst bad
patchStatus server Nothing
    | fails@(_:_) <- answered server [candidate' (state0 server, []), failure']
    = Rejected $ map fst fails

-- Detect progress
patchStatus server p
    | let filt = maybe (candidate' (state0 server, [])) patch' p
    , total:_ <- map (aTests . snd) $ answered server [filt, test' Nothing]
    , done <- nub $ mapMaybe (qTest . fst) $ answered server [filt, success']
    , todo <- total \\ done
    = if null todo && isNothing p then Accepted else Progressing done todo
patchStatus _ _ = Unknown
