{-# LANGUAGE RecordWildCards, ViewPatterns, TupleSections #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Brain
import Development.Bake.Server.Store
import Development.Bake.Server.Stats
import Development.Bake.Core.Type
import Development.Bake.Core.Message
import General.Web
import General.Extra
import General.HTML
import Data.List.Extra
import Data.Tuple.Extra
import Data.Either.Extra
import System.Time.Extra
import Data.Version
import Data.Maybe
import Data.Time.Calendar
import Control.Monad.Extra
import Data.Monoid
import Paths_bake
import qualified Data.Map as Map
import qualified Data.Set as Set
import Safe
import Prelude


web :: Prettys -> String -> [(String, String)] -> Memory -> IO String
web prettys admn (args admn -> a@Args{..}) mem@Memory{..} = recordIO $ fmap (first (\x -> ["web",x])) $ do
    shower@Shower{..} <- shower store prettys argsAdmin
    stats <- if argsStats then stats prettys mem else return mempty
    now <- getCurrentTime
    return $ (valueHTML &&& renderHTML . void) $ template $ do
        when (fatal /= []) $ do
            h2__ [class_ "bad"] $ str_ "Fatal error"
            p_ $ str_ "The continuous integration server has been suspeneded due to fatal errors:"
            ul_ $ mconcat $ map (li_ . str_) fatal
            hr_

        h1_ $
            (if argsEmpty a then id else a__ [href_ $ if argsAdmin then "?admin=" else "."]) $
            str_ "Bake Continuous Integration"

        if argsEmpty a{argsDate=Nothing} then do
            when (isNothing argsDate) $ do
                when paused $
                    p_ $ b_ (str_ "Paused") <> str_ ", new patches are paused until the queue is clear."
                failures shower mem
                progress shower mem

            p_ $ do
                str_ $ " Viewing " ++ maybe "yesterday and today" showDate argsDate ++ ": Goto "
                let shw d = showLink ("date=" ++ showDate d) $ str_ $ showDate d
                shw $ pred $ fromMaybe (timeToDate now) argsDate
                whenJust argsDate $ \d -> str_ ", " <> if timeToDate now == succ d then showLink "" $ str_ "today" else shw $ succ d

            table "No patches submitted" ["Submitted","Job","Status"] $
                map (\p -> rowPatch shower mem argsAdmin p) $
                map (either (Left . (id &&& storeState store)) (Right . (id &&& storePatch store))) $
                storeItemsDate store $ (dateToTime *** fmap dateToTime) $
                maybe (pred $ timeToDate now, Nothing) (\x -> (x, Just $ succ x)) argsDate

            unless (Map.null $ storeSkip store) $ do
                h2_ $ str_ "Skipped tests"
                ul_ $ fmap mconcat $ forM (Map.toList $ storeSkip store) $ \(test,author) -> li_ $ do
                    showTest (Just test) <> str_ (", by " ++ author ++ ".")
                    when argsAdmin $ str_ " " <> admin (DelSkip "admin" test) (str_ "Remove")
            h2_ $ str_ "Clients"
            table "No clients available" ["Name","Running"]
                (map (rowClient shower mem) $ Nothing : map Just (Map.toList clients))

            when argsAdmin $ do
                h2_ $ str_ "Admin"
                ul_ $ do
                    li_ $ if Set.null $ storeAlive store
                        then str_ "Cannot delete all patches, no patches available"
                        else admin (DelAllPatches "admin") $ str_ "Delete all patches"
                    li_ $ if null (Set.toList (storeAlive store) \\ snd active)
                        then str_ "Cannot requeue, no queued patches"
                        else admin (Requeue "admin") $ str_ "Reqeue"
                    li_ $ if paused
                        then admin (Unpause "admin") $ str_ "Unpause"
                        else admin (Pause "admin") $ str_ "Pause"
            return "home"

         else if argsStats then do
            stats
            return "stats"

         else if argsRaw then do
            str_ $ show mem
            return "raw"

         else if isJust argsServer then do
            let s = fromJust argsServer
            table "No server operations" ["Time","Job"] $
                map (("",) . rowUpdate shower mem) $
                    map (id &&& storeState store) $ storeStateList store
            whenJust s $ \s -> do
                h2_ $ str_ "Output"
                case storeStateFile store s of
                    Nothing -> p_ $ i_ $ str_ "File missing"
                    Just src -> pre_ $ str_ src
            return "server"

         else do
            let (keep,ignore) = splitAt 1000 $
                    map (\(t,q) -> (Nothing,t,q,Nothing)) (filter (argsFilter a . snd) running) ++
                    map (\(a,b,c,d) -> (Just a,b,c,Just d)) (storeRunList store argsClient argsTest argsState argsPatch argsRun)
            p_ $ let n = length keep in str_ $ "Found " ++ show n ++ " run" ++ ['s' | n /= 1] ++ (if null ignore then "" else ", truncated to 1000")
            table "No runs" ["Time","Job","Status"] $
                map (rowHistory shower mem) keep

            case keep of
                _ | Just s <- argsState, argsEmpty a{argsState=Nothing} ->
                    maybe' (storeExtra store $ Left s) (return "list") $ \(_, e) -> do
                        h2_ $ str_ "State information"; raw_ e
                        return "state"
                _ | [p] <- argsPatch, argsEmpty a{argsPatch=[]} ->
                    maybe' (storeExtra store $ Right p) (return "list") $ \(_, e) -> do
                        h2_ $ str_ "Patch information"; raw_ e
                        return "patch"
                [(Just run,_,Question{..},Just Answer{..})] -> do
                    when (argsAdmin && not aSuccess) $ whenJust qTest $ \t ->
                        p_ $ admin (AddSkip "admin" t) $ str_ "Skip test"
                    h2_ $ str_ "Output"
                    pre_ $ str_ $ fromMaybe "Missing" $ storeRunFile store run
                    return "output"
                _ -> return "list"


data Args = Args
    {argsState :: Maybe State
    ,argsDate :: Maybe Day
    ,argsPatch :: [Patch]
    ,argsClient :: Maybe Client
    ,argsTest :: Maybe (Maybe Test)
    ,argsRun :: Maybe RunId
    ,argsServer :: Maybe (Maybe State)
    ,argsAdmin :: Bool
    ,argsStats :: Bool
    ,argsRaw :: Bool
    }
    deriving (Show,Eq)

argsEmpty :: Args -> Bool
argsEmpty x = x{argsAdmin=False} == args "" []

args :: String -> [(String, String)] -> Args
args admn xs = Args
    (listToMaybe $ map toState $ ask "state")
    (listToMaybe $ map readDate $ ask "date")
    (map toPatch $ ask "patch")
    (listToMaybe $ map toClient $ ask "client")
    (listToMaybe $ map (\x -> if null x then Nothing else Just $ toTest x) $ ask "test")
    (listToMaybe $ map (readNote "run index") $ ask "run")
    (listToMaybe $ map (\x -> if null x then Nothing else Just $ toState x) $ ask "server")
    (any (if null admn then const True else (==) admn . encryptish) $ ask "admin")
    (not $ null $ ask "stats")
    (not $ null $ ask "raw")
    where ask x = map snd $ filter ((==) x . fst) xs

argsFilter :: Args -> Question -> Bool
argsFilter Args{..} Question{..} =
    isNothing argsRun &&
    maybe True (== qClient) argsClient &&
    maybe True (== qTest) argsTest &&
    case argsState of
        Just s -> (s,argsPatch) == qCandidate
        Nothing | null argsPatch -> True
        _ -> not $ disjoint argsPatch (snd qCandidate)


admin :: Message -> HTML -> HTML
admin (messageToInput -> Input parts args _) body = a__ [href_ url, class_ "admin"] body
    where url = intercalate "/" parts ++ "?" ++ intercalate "&" [url_ a ++ "=" ++ url_ b | (a,b) <- args]

table :: String -> [String] -> [(String, [HTML])] -> HTML
table zero cols [] = p_ $ str_ zero
table _ cols body = table_ $ do
    thead_ $ tr_ $ mconcat $ map (td_ . str_) cols
    tbody_ $ mconcat $ [tr__ [class_ cls] $ mconcat $ map td_ x | (cls,x) <- body]


data Shower = Shower
    {showLink :: String -> HTML -> HTML
    ,showPatch :: Patch -> HTML
    ,showExtra :: Either State Patch -> HTML
    ,showTest :: Maybe Test -> HTML
    ,showTestAt :: (State, [Patch]) -> Maybe Test -> HTML
    ,showQuestion :: Question -> HTML
    ,showClient :: Client -> HTML
    ,showState :: State -> HTML
    ,showCandidate :: (State, [Patch]) -> HTML
    ,showTime :: UTCTime -> HTML
    ,showThreads :: Int -> HTML
    }

shower :: Store -> Prettys -> Bool -> IO Shower
shower store Prettys{..} argsAdmin = do
    showRel <- showRelativeTime
    let shwState s | s == toState "" = span__ [class_ "bad" ] $ str_ $ "invalid state"
        shwState s = shwLink ("state=" ++ fromState s) $ str_ $ prettyState s
    let shwPatch p = shwLink ("patch=" ++ fromPatch p) $ str_ $ prettyPatch p
    return $ Shower
        {showLink = shwLink
        ,showPatch = shwPatch
        ,showState = shwState
        ,showCandidate = \(s,ps) -> do
            shwState s
            when (not $ null ps) $ str_ " plus " <> commas_ (map shwPatch ps)
        ,showExtra = \e -> raw_ $ maybe "" fst $ storeExtra store e
        ,showClient = \c -> shwLink ("client=" ++ url_ (fromClient c)) $ str_ $ fromClient c
        ,showTest = f Nothing Nothing []
        ,showTestAt = \(s,ps) -> f Nothing (Just s) ps
        ,showQuestion = \Question{..} -> f (Just qClient) (Just $ fst qCandidate) (snd qCandidate) qTest
        ,showTime = \x -> span__ [class_ "nobr"] $ str_ $ showUTCTime "%H:%M" x ++ " (" ++ showRel x ++ ")"
        ,showThreads = \i -> str_ $ show i ++ " thread" ++ ['s' | i /= 1]
        }
    where
        shwLink url = a__ [href_ $ (if argsAdmin then "?admin=&" else "?") ++ url]

        f c s ps t =
            shwLink (intercalate "&" parts) $ str_ $
            maybe "Preparing" prettyTest t
            where parts = ["client=" ++ url_ (fromClient c) | Just c <- [c]] ++
                          ["state=" ++ url_ (fromState s) | Just s <- [s]] ++
                          ["patch=" ++ url_ (fromPatch p) | p <- ps] ++
                          ["test=" ++ url_ (maybe "" fromTest t)]


template :: HTML_ a -> HTML_ a
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
                ,".active {background-color: #ffc;}"
                ,".dull {background-color: #e6e6e6;}"
                ,".pass {background-color: #dfc;}"
                ,".fail {background-color: #fcc;}"
                ,".nobr {white-space: nowrap;}"
                ,".red {background-color: #ffdddd;}"
                ,".green {background-color: #ddffdd;}"
                ,"#footer {margin-top: 40px; font-size: 80%;}"
                ,"hr {margin-bottom: 30px;}"
                ,".admin {color: darkorange; font-weight: bold;}"
                ]
        body_ $ do
            inner
            p__ [id_ "footer"] $
                a__ [href_ "https://github.com/ndmitchell/bake"] $
                    str_ $ "Copyright Neil Mitchell 2014-2015, version " ++ showVersion version
    return $ valueHTML inner


failures :: Shower -> Memory -> HTML
failures Shower{..} Memory{..} = when (ts /= []) $ do
    p_ $ str_ "Tracking down failures in:"
    ul_ $ mconcat $ map (li_ . showTest) ts
    where
        ts = Set.toList $ failed `Set.difference` reject
        failed = poFail $ storePoint store active
        reject = Set.unions $ mapMaybe (fmap (Map.keysSet . snd) . paReject . storePatch store) $ snd active


progress :: Shower -> Memory -> HTML
progress Shower{..} Memory{..}
    | null (snd active), Just todo <- poTodo, Set.size done == Set.size todo + 1 = return () -- Idle on a state
    | Just t <- poTodo = p_ $ b_ (str_ "Testing") <>
        str_ (", done " ++ show (Set.size done) ++ " tests out of " ++ show (Set.size t + 1) ++ superset)
    | isRunning = p_ $ b_ (str_ "Preparing") <>
        str_ (", getting ready to test" ++ superset)
    | otherwise = return ()
    where
        PointInfo{..} = storePoint store active
        done = Set.union poPass poFail
        superset = let x = storeSupersetPass store active `Set.difference` catMaybesSet done
                       x2 = maybe x (Set.intersection x) poTodo
                   in if Set.null x2 then "" else ", and done " ++ show (Set.size x2) ++ " in a superset"
        isRunning = any ((==) active . qCandidate . snd) running


showAnswer :: Maybe Answer -> HTML
showAnswer Nothing = i_ $ str_ $ "Running..."
showAnswer (Just Answer{..}) =
    if aSuccess then span__ [class_ "good"] $ str_ $ "Succeeded in " ++ maybe "no time" showDuration aDuration
                else span__ [class_ "bad" ] $ str_ $ "Failed in "    ++ maybe "no time" showDuration aDuration


rowHistory :: Shower -> Memory -> (Maybe RunId, UTCTime, Question, Maybe Answer) -> (String, [HTML])
rowHistory Shower{..} Memory{..} (run, t, q@Question{..}, a) = ("", [showTime t, body, showAnswer a])
    where
        body = do
            str_ "With " <> showCandidate qCandidate
            br_
            str_ "Test " <> showQuestion q <> str_ " on " <> showClient qClient
            str_ " with " <> showThreads qThreads


rowUpdate :: Shower -> Memory -> (State,StateInfo) -> [HTML]
rowUpdate Shower{..} Memory{..} (s,StateInfo{..}) = [showTime stCreated, body]
    where
        body = do
            showLink ("server=" ++ fromState s) $ str_ $ if isNothing stSource then "Initialised" else "Updated"
            br_
            whenJust stSource $ \src -> str_ "With " <> commas_ (map showPatch $ snd src)
            str_ "To " <> showState s


rowPatch :: Shower -> Memory -> Bool -> Either (State, StateInfo) (Patch,PatchInfo) -> (String, [HTML])
rowPatch Shower{..} mem@Memory{..} argsAdmin info = (code, [showTime time, state, body <> special])
    where
        failed = case info of
            Right (p, PatchInfo{..}) -> fmap (second Map.toList) paReject
            Left (s, StateInfo{..}) -> if Set.null x then Nothing else Just (stCreated, map (,(s, [])) $ Set.toList x)
                where x = poFail $ storePoint store (s, [])

        code | Right (p,_) <- info, any (isSuffixOf [p] . snd . qCandidate . snd) running = "active"
             | Left (s,_) <- info, (s,[]) `elem` map (qCandidate . snd) running = "active"
             | isJust failed = "fail"
             | Right (_, PatchInfo{..}) <- info, isJust paSupersede || isNothing paStart = "dull"
             | Right (_, PatchInfo{..}) <- info, isJust paMerge || isJust paPlausible  = "pass"
             | Left (s,_) <- info, fst active /= s = "pass"
             | Left (s,_) <- info, PointInfo{poTodo=Just todo,..} <- storePoint store (s,[])
                 , Set.size todo + 1 == Set.size (poPass `Set.union` poFail)  = "pass"
             | otherwise = ""

        body
            | Just (time, xs) <- failed = do
                span__ [class_ "bad"] $ str_ $ if isLeft info then "Failed" else "Rejected"
                str_ " at " <> showTime time
                when (xs /= []) br_
                span__ [class_ "info"] $ commasLimit_ 3 [showTestAt sps t | (t,sps) <- xs]
            | Right (_, p) <- info, paAlive p && isNothing (paStart p) = str_ "Queued"
            | Right (_, PatchInfo{paSupersede=Just t}) <- info = str_ "Superseded at " <> showTime t
            | Right (_, PatchInfo{paMerge=Just t}) <- info = do
                span__ [class_ "good"] $ str_ "Merged"
                str_ " at " <> showTime t
            | Right (_, PatchInfo{paPlausible=Just t}) <- info = do
                span__ [class_ "good"] $ str_ "Plausible"
                str_ " at " <> showTime t
            | Left (s,_) <- info, fst active /= s = span__ [class_ "good"] $ str_ "Passed"
            | Left (s,_) <- info, PointInfo{poTodo=Just todo,..} <- storePoint store (s,[])
                , Set.size todo + 1 == Set.size (poPass `Set.union` poFail)  = span__ [class_ "good"] $ str_ "Passed"
            | otherwise = str_ "Active"

        special
            | argsAdmin, Right (p, pi) <- info =
                if paAlive pi then
                    do br_; admin (DelPatch "admin" p) $ str_ "Delete"
                else if isNothing $ paMerge pi then
                    do br_; admin (AddPatch "admin" $ toPatch $ '\'' : fromPatch p) $ str_ "Retry"
                else
                    mempty
            | otherwise = mempty

        state = do
            either ((str_ "State " <>) . showState . fst) ((str_ "Patch " <>) . showPatch . fst) info
            whenRight info $ \(pa, PatchInfo{..}) -> str_ $ " by " ++ paAuthor
            br_
            span__ [class_ "info"] $ showExtra $ either (Left . fst) (Right . fst) info

        time = either (stCreated . snd) (paQueued . snd) info


rowClient :: Shower -> Memory -> Maybe (Client, ClientInfo) -> (String, [HTML])
rowClient Shower{..} Memory{..} (Just (c, ClientInfo{..})) = ((if ciAlive then "" else "dull"),) $
    [showLink ("client=" ++ url_ (fromClient c)) $ str_ $ fromClient c
    ,if null xs then i_ $ str_ "None" else mconcat $ intersperse br_ xs]
    where xs = reverse [showQuestion q <> str_ " started " <> showTime t | (t,q) <- running, qClient q == c]
rowClient Shower{..} Memory{..} Nothing = ("",) $
    [showLink "server=" $ i_ $ str_ "Server"
    ,showLink ("server=" ++ fromState (fst active)) (str_ $ if isNothing stSource then "Initialised" else "Updated") <>
        str_ " finished " <> showTime stCreated]
    where StateInfo{..} = storeState store $ fst active
