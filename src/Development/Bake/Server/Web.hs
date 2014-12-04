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
import General.HTML
import General.DelayCache
import General.Str
import Data.List.Extra
import Data.Tuple.Extra
import System.Time.Extra
import Data.Version
import Data.Maybe
import Control.Monad
import Data.Monoid
import Paths_bake
import Development.Bake.Server.Query
import qualified Data.Map as Map


web :: Oven State Patch Test -> [(String, String)] -> Server -> IO Output
web oven@Oven{..} args server@Server{..} = do
    extra <- askDelayCache extra
    shower <- shower extra oven
    return $ OutputHTML $ renderHTML $ template $
        if null args then do
            h1_ $ str_ "Bake Continuous Integration"
            when (fatal /= []) $ do
                h2__ [class_ "bad"] $ str_ "Fatal error"
                p_ $ str_ "The continuous integration server has been suspeneded due to fatal errors:"
                ul_ $ mconcat $ map (li_ . str_) fatal
            h2_ $ str_ "Patches"
            failures shower server
            table "No patches submitted" ["Patch","Time","Status"]
                (map (patch shower server) $ nub (map (Just . snd) submitted) ++ [Nothing])
            h2_ $ str_ "Clients"
            table "No clients available" ["Name","Running"]
                (map (client shower server) $ Map.keys pings)
        else do
            let ask x = map snd $ filter ((==) x . fst) args
            h1_ $ a__ [href_ "?"] $ str_ "Bake Continuous Integration"
            runs shower server (\Question{..} ->
                let or0 xs = if null xs then True else or xs in
                or0 [qClient == Client c | c <- ask "client"] &&
                or0 [qTest == if t == "" then Nothing else Just (Test t) | t <- ask "test"] &&
                case ask "state" of
                    [] -> or0 [Patch p `elem` snd qCandidate | p <- ask "patch"]
                    s:_ -> qCandidate == (State s, map Patch $ ask "patch"))
            case ask "patch" of
                [p] | null $ ask "test", Just (_, e) <- extra $ Right $ Patch p ->
                        do h2_ $ str_ "Patch information"; raw_ $ strUnpack e
                _ -> mempty


table :: String -> [String] -> [[HTML]] -> HTML
table zero cols [] = p_ $ str_ zero
table _ cols body = table_ $ do
    thead_ $ tr_ $ mconcat $ map (td_ . str_) cols
    tbody_ $ mconcat $ [tr_ $ mconcat $ map td_ x | x <- body]


data Shower = Shower
    {showPatch :: Patch -> HTML
    ,showExtra :: Either State Patch -> HTML
    ,showTest :: Maybe Test -> HTML
    ,showTestPatch :: Patch -> Maybe Test -> HTML
    ,showTestQuestion :: Question -> HTML
    ,showState :: State -> HTML
    ,showTime :: Timestamp -> HTML
    }

showThreads i = show i ++ " thread" ++ ['s' | i /= 1]

shower :: (Either State Patch -> Maybe (Str, Str)) -> Oven State Patch Test -> IO Shower
shower extra Oven{..} = do
    showTimestamp <- showRelativeTimestamp
    return $ Shower
        {showPatch = \p -> a__ [href_ $ "?patch=" ++ fromPatch p, class_ "patch"] $ str_ $ stringyPretty ovenStringyPatch p
        ,showExtra = \e -> raw_ $ maybe "" (strUnpack . fst) $ extra e
        ,showState = \s -> a__ [href_ $ "?state=" ++ fromState s, class_ "state"] $ str_ $ stringyPretty ovenStringyState s
        ,showTest = f Nothing Nothing []
        ,showTestPatch = \p -> f Nothing Nothing [p]
        ,showTestQuestion = \Question{..} -> f (Just qClient) (Just $ fst qCandidate) (snd qCandidate) qTest
        ,showTime = span__ [class_ "nobr"] . str_ . showTimestamp
        }
    where
        f c s ps t =
            a__ [href_ $ "?" ++ intercalate "&" parts] $ str_ $
            maybe "Preparing" (stringyPretty ovenStringyTest) t
            where parts = ["client=" ++ fromClient c | Just c <- [c]] ++
                          ["state=" ++ fromState s | Just s <- [s]] ++
                          ["patch=" ++ fromPatch p | p <- ps] ++
                          ["test=" ++ maybe "" fromTest t]

template :: HTML -> HTML
template inner = do
    raw_ "<!HTML>"
    html_ $ do
        head_ $ do
            title_ $ str_ "Bake Continuous Integration"
            link__ [rel_ "shortcut icon", type_ "image/x-icon", href_ "html/favicon.ico"]
            style__ [type_ "text/css"] $ unlines
                ["body, td {font-family: sans-serif; font-size: 10pt;}"
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
                ,".red {background-color: #ffdddd;}"
                ,".green {background-color: #ddffdd;}"
                ,"#footer {margin-top: 40px; font-size: 80%;}"
                ]
        body_ $ do
            inner
            p__ [id_ "footer"] $
                a__ [href_ "https://github.com/ndmitchell/bake"] $
                    str_ $ "Copyright Neil Mitchell 2014, version " ++ showVersion version


failures :: Shower -> Server -> HTML
failures Shower{..} server = when (xs /= []) $ do
    p_ $ str_ "Tracking down failures in:"
    ul_ $ mconcat $ map (li_ . showTest) xs
    where xs = nub $ map fst $ targetFailures server


runs :: Shower -> Server -> (Question -> Bool) -> HTML
runs Shower{..} Server{..} pred = do
    table "No runs" ["Time","Question","Answer"]
        [[showTime t, showQuestion q, showAnswer a] | (t,q,a) <- good]
    case good of
        [(_,_,Just Answer{..})] -> pre_ $ str_ $ strUnpack aStdout
        _ -> mempty
    where
        good = filter (pred . snd3) history
        showQuestion q@Question{..} = do
            str_ "With " <> showState (fst qCandidate)
            when (not $ null $ snd qCandidate) $
                str_ " plus " <> commas_ (map showPatch $ snd qCandidate)
            br_
            str_ "Test " <> showTestQuestion q <> str_ " on "
            str_ $ fromClient qClient ++ " with " ++ showThreads qThreads
        showAnswer Nothing = i_ $ str_ $ "Running..."
        showAnswer (Just Answer{..}) =
            if aSuccess then span__ [class_ "good"] $ str_ $ "Succeeded in " ++ showDuration aDuration
                        else span__ [class_ "bad" ] $ str_ $ "Failed in "    ++ showDuration aDuration


patch :: Shower -> Server -> Maybe Patch -> [HTML]
patch Shower{..} server@Server{..} p =
    [do
        maybe (str_ "Initial state " <> showState s0) showPatch p
        str_ $ " by " ++ commasLimit 3 (nub $ Map.findWithDefault [] p authors)
        br_
        span__ [class_ "info"] $ showExtra $ maybe (Left s0) Right p

    ,maybe mempty (showTime . fst) $ find ((==) p . Just . snd) submitted

    ,case patchStatus server p of
        Accepted -> span__ [class_ "good"] $ str_ "Success"
        Unknown -> str_ "Testing (passed 0 of ?)" <> running
        Paused -> str_ "Paused"
        Rejected xs -> span__ [class_ "bad"] (str_ "Rejected") <> br_ <>
            span__ [class_ "info"] (commasLimit_ 3 $ map showTestQuestion xs)
        Progressing done todo -> do
            str_ $ "Testing (passed " ++ show (length done + 1) ++ " of " ++ show (length (done++todo) + 1) ++ ")"
            running
    ]
    where
        s0 = state0 server
        running | null xs = mempty
                | otherwise = br_ <> span__ [class_ "info"] (commasLimit_ 3 items)
            where xs = unanswered server [maybe (candidate' (s0,[])) patch' p]
                  (yes,no) = partition (maybe null (isSuffixOf . return) p . snd . qCandidate) xs
                  items = map (b_ . showTestQuestion) yes ++ map showTestQuestion no


client :: Shower -> Server -> Client -> [HTML]
client Shower{..} server c =
    [a__ [href_ $ "?client=" ++ fromClient c] $ str_ $ fromClient c
    ,if null target then i_ $ str_ "None"
     else commas_ $ map showTestQuestion target]
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
    | p `elem` concatMap (maybe [] snd . thd3) (updates server) = Accepted
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
