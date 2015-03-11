{-# LANGUAGE RecordWildCards, ViewPatterns, TupleSections #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Web(
    web
    ) where

import Development.Bake.Server.Type
import Development.Bake.Server.Stats
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
import Control.Applicative
import Control.Monad.Extra
import Data.Monoid
import Paths_bake
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude


web :: Oven State Patch Test -> [(String, String)] -> Server -> IO String
web oven@Oven{..} (args -> a@Args{..}) server@Server{..} = recordIO $ fmap (first (\x -> ["web",x])) $ do
    extra <- askDelayCache extra
    shower <- shower extra oven argsAdmin
    stats <- if argsStats then stats server else return mempty
    return $ (valueHTML &&& renderHTML . void) $ template $ do
        let noargs = argsEmpty a

        when (fatal /= []) $ do
            h2__ [class_ "bad"] $ str_ "Fatal error"
            p_ $ str_ "The continuous integration server has been suspeneded due to fatal errors:"
            ul_ $ mconcat $ map (li_ . str_) fatal
            hr_

        h1_ $
            (if noargs then id else a__ [href_ $ if argsAdmin then "?admin=" else "."]) $
            str_ "Bake Continuous Integration"

        if noargs then do
            when paused $
                p_ $ b_ (str_ "Paused") <> str_ ", new patches are paused until the queue is clear."
            failures shower server
            let toPoint p | ps@(_:_) <- dropWhileEnd (/= p) (snd target) = Just $ newPoint server (fst target, ps)
                          | otherwise = Nothing
            let active = [fmap snd $ unsnoc $ snd $ qCandidate q | (t,q,Nothing) <- history]
            table "No patches submitted" ["Time","Job","Status"]
                (map (\p -> rowPatch shower server argsAdmin (p `elem` active) (maybe Nothing toPoint p) p) $
                    nubOrd (map (Just . snd) submitted) ++ [Nothing])
            h2_ $ str_ "Clients"
            table "No clients available" ["Name","Running"]
                (map (rowClient shower server) $ Nothing : map Just (Map.keys pings))

            when argsAdmin $ do
                h2_ $ str_ "Admin"
                ul_ $ do
                    li_ $ if null (snd target) && null queued
                        then str_ "Cannot delete all patches, no patches queued"
                        else admin (DelAllPatches "admin") $ str_ "Delete all patches"
                    li_ $ if paused
                        then admin (Unpause "admin") $ str_ "Unpause"
                        else admin (Pause "admin") $ str_ "Pause"
            return "home"

         else if argsStats then do
            stats
            return "stats"

         else if argsRaw then do
            str_ $ show server
            return "raw"

         else if isJust argsServer then do
            let s = fromJust argsServer
            table "No server operations" ["Time","Job","Status"] $
                map (("",) . rowUpdate shower server) $
                    filter (maybe (const True) (==) s . fst) $
                    reverse $ zip [0..] $ reverse updates
            whenJust s $ \s -> do
                h2_ $ str_ "Output"
                pre_ $ str_ $ strUnpack $ aStdout $ uiAnswer $ reverse updates !! s
            return "server"

         else do
            let xs = filter (argsFilter a . snd3) history
            table "No runs" ["Time","Job","Status"] $
                map (("",) . rowHistory shower server) xs

            case xs of
                _ | Just s <- argsState, argsEmpty a{argsState=Nothing} ->
                    maybeM (return "list") (extra $ Left s) $ \(_, e) -> do
                        h2_ $ str_ "State information"; raw_ $ strUnpack e
                        return "state"
                _ | [p] <- argsPatch, argsEmpty a{argsPatch=[]} ->
                    maybeM (return "list") (extra $ Right p) $ \(_, e) -> do
                        h2_ $ str_ "Patch information"; raw_ $ strUnpack e
                        return "patch"
                [(_,_,Just Answer{..})] -> do
                    h2_ $ str_ "Output"
                    pre_ $ str_ $ strUnpack aStdout
                    return "output"
                _ -> return "list"

maybeM :: m b -> Maybe a -> (a -> m b) -> m b
maybeM nothing Nothing _ = nothing
maybeM _ (Just x) just = just x

data Args = Args
    {argsState :: Maybe State
    ,argsPatch :: [Patch]
    ,argsClient :: Maybe Client
    ,argsTest :: Maybe (Maybe Test)
    ,argsServer :: Maybe (Maybe Int)
    ,argsAdmin :: Bool
    ,argsStats :: Bool
    ,argsRaw :: Bool
    }
    deriving (Show,Eq)

argsEmpty :: Args -> Bool
argsEmpty x = x{argsAdmin=False} == args []

args :: [(String, String)] -> Args
args xs = Args
    (listToMaybe $ map State $ ask "state")
    (map Patch $ ask "patch")
    (listToMaybe $ map Client $ ask "client")
    (listToMaybe $ map (\x -> if null x then Nothing else Just $ Test x) $ ask "test")
    (listToMaybe $ map (\x -> if null x then Nothing else Just $ read x) $ ask "server")
    (not $ null $ ask "admin")
    (not $ null $ ask "stats")
    (not $ null $ ask "raw")
    where ask x = map snd $ filter ((==) x . fst) xs

argsFilter :: Args -> Question -> Bool
argsFilter Args{..} Question{..} =
    maybe True (== qClient) argsClient &&
    maybe True (== qTest) argsTest &&
    case argsState of
        Just s -> (s,argsPatch) == qCandidate
        Nothing | null argsPatch -> True
        _ -> not $ disjoint argsPatch (snd qCandidate)


admin :: Message -> HTML -> HTML
admin (messageToInput -> Input parts args _) body = a__ [href_ url, class_ "admin"] body
    where url = intercalate "/" parts ++ "?" ++ intercalate "&" [a ++ "=" ++ b | (a,b) <- args]

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
    ,showQuestion :: Question -> HTML
    ,showClient :: Client -> HTML
    ,showState :: State -> HTML
    ,showCandidate :: (State, [Patch]) -> HTML
    ,showTime :: UTCTime -> HTML
    ,showThreads :: Int -> HTML
    }

shower :: (Either State Patch -> Maybe (Str, Str)) -> Oven State Patch Test -> Bool -> IO Shower
shower extra Oven{..} argsAdmin = do
    showTime <- showRelativeTime
    let shwState (State "") = span__ [class_ "bad" ] $ str_ $ "invalid state"
        shwState s = shwLink ("state=" ++ fromState s) $ str_ $ stringyPretty ovenStringyState s
    let shwPatch p = shwLink ("patch=" ++ fromPatch p) $ str_ $ stringyPretty ovenStringyPatch p
    return $ Shower
        {showLink = shwLink
        ,showPatch = shwPatch
        ,showState = shwState
        ,showCandidate = \(s,ps) -> do
            shwState s
            when (not $ null ps) $ str_ " plus " <> commas_ (map shwPatch ps)
        ,showExtra = \e -> raw_ $ maybe "" (strUnpack . fst) $ extra e
        ,showClient = \c -> shwLink ("client=" ++ fromClient c) $ str_ $ fromClient c
        ,showTest = f Nothing Nothing []
        ,showQuestion = \Question{..} -> f (Just qClient) (Just $ fst qCandidate) (snd qCandidate) qTest
        ,showTime = span__ [class_ "nobr"] . str_ . showTime
        ,showThreads = \i -> str_ $ show i ++ " thread" ++ ['s' | i /= 1]
        }
    where
        shwLink url = a__ [href_ $ (if argsAdmin then "?admin=&" else "?") ++ url]

        f c s ps t =
            shwLink (intercalate "&" parts) $ str_ $
            maybe "Preparing" (stringyPretty ovenStringyTest) t
            where parts = ["client=" ++ fromClient c | Just c <- [c]] ++
                          ["state=" ++ fromState s | Just s <- [s]] ++
                          ["patch=" ++ fromPatch p | p <- ps] ++
                          ["test=" ++ maybe "" fromTest t]


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


failures :: Shower -> Server -> HTML
failures Shower{..} server = when (xs /= []) $ do
    p_ $ str_ "Tracking down failures in:"
    ul_ $ mconcat $ map (li_ . showTest) xs
    where xs = Map.keys $ poFail $ Map.findWithDefault mempty (newPoint server (target server)) $ pointInfo server


showAnswer :: Maybe Answer -> HTML
showAnswer Nothing = i_ $ str_ $ "Running..."
showAnswer (Just Answer{..}) =
    if aSuccess then span__ [class_ "good"] $ str_ $ "Succeeded in " ++ showDuration aDuration
                else span__ [class_ "bad" ] $ str_ $ "Failed in "    ++ showDuration aDuration


rowHistory :: Shower -> Server -> (UTCTime, Question, Maybe Answer) -> [HTML]
rowHistory Shower{..} Server{..} (t, q@Question{..}, a) = [showTime t, body, showAnswer a]
    where
        body = do
            str_ "With " <> showCandidate qCandidate
            br_
            str_ "Test " <> showQuestion q <> str_ " on " <> showClient qClient
            str_ " with " <> showThreads qThreads


rowUpdate :: Shower -> Server -> (Int,UpdateInfo) -> [HTML]
rowUpdate Shower{..} Server{..} (i,UpdateInfo{..}) = [showTime uiTime, body, showAnswer $ Just uiAnswer]
    where
        body = do
            showLink ("server=" ++ show i) $ str_ $ if isNothing uiPrevious then "Initialised" else "Updated"
            br_
            whenJust uiPrevious $ \c -> str_ "From " <> showCandidate c <> br_
            str_ "To " <> showState uiState


rowPatch :: Shower -> Server -> Bool -> Bool -> Maybe Point -> Maybe Patch -> (String, [HTML])
rowPatch Shower{..} server@Server{..} argsAdmin active point patch = second (\x -> [time,state,x <> special]) $
    case () of
        _ | root <- Map.findWithDefault mempty (newPoint server target) pointInfo
          , Just me <- if patch == Nothing && target == (state0 server, []) && Map.null (poFail root) && maybe False (\x -> Set.size x + 1 == Map.size (poPass root)) (poTests root) then Just root
                       else (\p -> Map.findWithDefault mempty p pointInfo) <$> point
            -> ((if not $ Set.null $ Map.keysSet (poFail me) `Set.intersection` Map.keysSet (poFail root) then "fail"
                 else if not $ Set.null $ Map.keysSet (poPass me) `Set.intersection` Map.keysSet (poFail root) then "pass"
                 else if active then "active" else "")
               ,span__ [class_ "nobr"] $ str_ $
                    "Testing (passed " ++ show (maybe (Map.size $ poPass me) (\p -> Set.size $ paPass $ Map.findWithDefault mempty p patchInfo) patch) ++
                    " of " ++ maybe "?" (show . Set.size) (poTests root) ++ ")")
        _ | Just p <- patch, p `elem` concatMap (maybe [] snd . uiPrevious) updates
            -> ("dull", span__ [class_ "good"] $ str_ "Success")
        _ | Just p <- patch, p `elem` queued
            -> ("dull", str_ "Queued")
        _ | Just p <- patch, xs <- concatMap (map snd3 . take 1) $ Map.elems $ paReject $ Map.findWithDefault mempty p patchInfo
            -> ("dull",) $ do
                span__ [class_ "bad"] $ str_ "Rejected"
                when (xs /= []) br_
                span__ [class_ "info"] $ commasLimit_ 3 $ map showQuestion xs
        _ | xs@(_:_) <- concatMap (map snd3 . take 1) $ Map.elems $ poReject $ Map.findWithDefault mempty (newPoint server (state0 server,[])) pointInfo
            -> ("dull",) $ do
                span__ [class_ "bad"] $ str_ "Failed"
        _ -> ("dull", span__ [class_ "good"] $ str_ "Success")
    where
        time | Just p <- patch = maybe mempty (showTime . fst) $ find ((==) p . snd) submitted
             | otherwise = showTime $ uiTime $ last updates

        state = do
            maybe (str_ "Initial state " <> showState s0) ((str_ "Patch " <>) . showPatch) patch
            str_ $ " by " ++ commasLimit 3 (nubOrd $ Map.findWithDefault [] patch authors)
            br_
            span__ [class_ "info"] $ showExtra $ maybe (Left s0) Right patch

        s0 = state0 server

        special | argsAdmin, Just p <- patch =
            if p `elem` snd target || p `elem` queued then
                do br_; admin (DelPatch "admin" p) $ str_ "Delete"
            else if p `notElem` concatMap (maybe [] snd . uiPrevious) updates then
                do br_; admin (AddPatch "admin" p) $ str_ "Retry"
            else
                mempty
                | otherwise = mempty


rowClient :: Shower -> Server -> Maybe Client -> (String, [HTML])
rowClient Shower{..} server (Just c) = ((if maybe False piAlive $ Map.lookup c $ pings server then "" else "dull"),) $
    [showLink ("client=" ++ fromClient c) $ str_ $ fromClient c
    ,if null xs then i_ $ str_ "None" else mconcat $ intersperse br_ xs]
    where xs = reverse [showQuestion q <> str_ " started " <> showTime t | (t,q,Nothing) <- history server, qClient q == c]
rowClient Shower{..} Server{..} Nothing = ("",) $
    [showLink "server=" $ i_ $ str_ "Server"
    ,showLink ("server=" ++ show (length updates - 1))
        (str_ $ if length updates == 1 then "Initialised" else "Updated") <>
     str_ " finished " <> showTime (uiTime $ head updates)]
