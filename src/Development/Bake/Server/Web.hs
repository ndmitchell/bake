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
import Control.Monad.Extra
import Data.Monoid
import Paths_bake
import Development.Bake.Server.Query
import qualified Data.Map as Map


web :: Oven State Patch Test -> [(String, String)] -> Server -> IO Output
web oven@Oven{..} (args -> a@Args{..}) server@Server{..} = do
    extra <- askDelayCache extra
    shower <- shower extra oven argsAdmin
    return $ OutputHTML $ renderHTML $ template $ do
        let noargs = argsEmpty a

        when (fatal /= []) $ do
            h2__ [class_ "bad"] $ str_ "Fatal error"
            p_ $ str_ "The continuous integration server has been suspeneded due to fatal errors:"
            ul_ $ mconcat $ map (li_ . str_) fatal
            hr_

        h1_ $
            (if noargs then id else a__ [href_ $ if argsAdmin then "?admin=" else "."]) $
            str_ "Bake Continuous Integration"

        when noargs $ do
            when (isJust paused) $
                p_ $ b_ (str_ "Paused") <> str_ ", new patches are paused until the queue is clear."
            failures shower server
            table "No patches submitted" ["Time","Job","Status"]
                (map (rowPatch shower server argsAdmin) $ nub (map (Just . snd) submitted) ++ [Nothing])
            h2_ $ str_ "Clients"
            table "No clients available" ["Name","Running"]
                (map (rowClient shower server) $ Nothing : map Just (Map.keys pings))

            when argsAdmin $ do
                h2_ $ str_ "Admin"
                ul_ $ do
                    li_ $ if null (snd target) && isNothing paused
                        then str_ "Cannot delete all patches, no patches queued"
                        else admin (DelAllPatches "admin") $ str_ "Delete all patches"
                    li_ $ if null (snd target)
                        then str_ "Cannot pause, no active targets"
                        else if isJust paused then str_ "Cannot pause, already paused"
                        else admin (Pause "admin") $ str_ "Pause"
                    li_ $ if isNothing paused
                        then str_ "Cannot unpause, not paused"
                        else admin (Unpause "admin") $ str_ "Unpause"

        whenJust argsServer $ \s -> do
            table "No server operations" ["Time","Job","Status"] $
                map (rowUpdate shower server) $
                    filter (maybe (const True) (==) s . fst) $
                    reverse $ zip [0..] $ reverse updates
            whenJust s $ \s -> do
                h2_ $ str_ "Output"
                pre_ $ str_ $ strUnpack $ aStdout $ snd $ fst3 $ reverse updates !! s

        when (isNothing argsServer && not noargs) $ do
            let xs = filter (argsFilter a . snd3) history
            table "No runs" ["Time","Job","Status"] $
                map (rowHistory shower server) xs

            case xs of
                _ | Just s <- argsState, argsEmpty a{argsState=Nothing} ->
                    whenJust (extra $ Left s) $ \(_, e) -> do
                        h2_ $ str_ "State information"; raw_ $ strUnpack e
                _ | [p] <- argsPatch, argsEmpty a{argsPatch=[]} ->
                    whenJust (extra $ Right p) $ \(_, e) -> do
                        h2_ $ str_ "Patch information"; raw_ $ strUnpack e
                [(_,_,Just Answer{..})] -> do
                    h2_ $ str_ "Output"
                    pre_ $ str_ $ strUnpack aStdout
                _ -> mempty

data Args = Args
    {argsState :: Maybe State
    ,argsPatch :: [Patch]
    ,argsClient :: Maybe Client
    ,argsTest :: Maybe (Maybe Test)
    ,argsServer :: Maybe (Maybe Int)
    ,argsAdmin :: Bool
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

table :: String -> [String] -> [[HTML]] -> HTML
table zero cols [] = p_ $ str_ zero
table _ cols body = table_ $ do
    thead_ $ tr_ $ mconcat $ map (td_ . str_) cols
    tbody_ $ mconcat $ [tr_ $ mconcat $ map td_ x | x <- body]


data Shower = Shower
    {showLink :: String -> HTML -> HTML
    ,showPatch :: Patch -> HTML
    ,showExtra :: Either State Patch -> HTML
    ,showTest :: Maybe Test -> HTML
    ,showQuestion :: Question -> HTML
    ,showClient :: Client -> HTML
    ,showState :: State -> HTML
    ,showCandidate :: (State, [Patch]) -> HTML
    ,showTime :: Timestamp -> HTML
    ,showThreads :: Int -> HTML
    }

shower :: (Either State Patch -> Maybe (Str, Str)) -> Oven State Patch Test -> Bool -> IO Shower
shower extra Oven{..} argsAdmin = do
    showTimestamp <- showRelativeTimestamp
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
        ,showTime = span__ [class_ "nobr"] . str_ . showTimestamp
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
                ,"hr {margin-bottom: 30px;}"
                ,".admin {color: darkorange; font-weight: bold;}"
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


showAnswer :: Maybe Answer -> HTML
showAnswer Nothing = i_ $ str_ $ "Running..."
showAnswer (Just Answer{..}) =
    if aSuccess then span__ [class_ "good"] $ str_ $ "Succeeded in " ++ showDuration aDuration
                else span__ [class_ "bad" ] $ str_ $ "Failed in "    ++ showDuration aDuration


rowHistory :: Shower -> Server -> (Timestamp, Question, Maybe Answer) -> [HTML]
rowHistory Shower{..} Server{..} (t, q@Question{..}, a) = [showTime t, body, showAnswer a]
    where
        body = do
            str_ "With " <> showCandidate qCandidate
            br_
            str_ "Test " <> showQuestion q <> str_ " on " <> showClient qClient
            str_ " with " <> showThreads qThreads


rowUpdate :: Shower -> Server -> (Int,((Timestamp,Answer), State, Maybe (State, [Patch]))) -> [HTML]
rowUpdate Shower{..} Server{..} (i,((t,a), to, from)) = [showTime t, body, showAnswer $ Just a]
    where
        body = do
            showLink ("server=" ++ show i) $ str_ $ if isNothing from then "Initialised" else "Updated"
            br_
            whenJust from $ \c -> str_ "From " <> showCandidate c <> br_
            str_ "To " <> showState to


rowPatch :: Shower -> Server -> Bool -> Maybe Patch -> [HTML]
rowPatch Shower{..} server@Server{..} argsAdmin p =
    [case p of
        Nothing -> showTime $ fst $ fst3 $ last updates
        Just p -> maybe mempty (showTime . fst) $ find ((==) p . snd) submitted

    ,do
        maybe (str_ "Initial state " <> showState s0) ((str_ "Patch " <>) . showPatch) p
        str_ $ " by " ++ commasLimit 3 (nub $ Map.findWithDefault [] p authors)
        br_
        span__ [class_ "info"] $ showExtra $ maybe (Left s0) Right p

    ,(<> special) $ case patchStatus server p of
        Accepted -> span__ [class_ "good"] $ str_ "Success"
        Unknown -> str_ "Testing (passed 0 of ?)" <> running
        Paused -> str_ "Paused"
        Rejected xs -> span__ [class_ "bad"] (str_ "Rejected") <> when (xs /= []) br_ <>
            span__ [class_ "info"] (commasLimit_ 3 $ map showQuestion xs)
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
                  items = map (b_ . showQuestion) yes ++ map showQuestion no

        special | argsAdmin, Just p <- p =
            if p `elem` snd target || p `elem` fromMaybe [] paused then
                do br_; admin (DelPatch "admin" p) $ str_ "Delete"
            else if p `notElem` concatMap (maybe [] snd . thd3) updates then
                do br_; admin (AddPatch "admin" p) $ str_ "Retry"
            else
                mempty
                | otherwise = mempty


rowClient :: Shower -> Server -> Maybe Client -> [HTML]
rowClient Shower{..} server (Just c) =
    [showLink ("client=" ++ fromClient c) $ str_ $ fromClient c
    ,if null xs then i_ $ str_ "None" else mconcat $ intersperse br_ xs]
    where xs = [showQuestion q <> str_ " started " <> showTime t | (t,q,Nothing) <- history server, qClient q == c]
rowClient Shower{..} Server{..} Nothing =
    [showLink "server=" $ i_ $ str_ "Server"
    ,showLink ("server=" ++ show (length updates - 1))
        (str_ $ if length updates == 1 then "Initialised" else "Updated") <>
     str_ " finished " <> showTime (fst $ fst3 $ head updates)]


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
    | p `elem` fromMaybe [] (paused server) = Paused
patchStatus server Nothing
    | length (updates server) > 1 = Accepted

-- Detect rejection
patchStatus server (Just p)
    -- we may have previously failed, but been requeued, so if we're active don't hunt for reject
    | p `notElem` snd (target server)
    , bad <- answered server [lastPatch' p, blame']
    = Rejected $ nub $ map fst bad
    -- note we may be rejected with null bad, could be due to admin action
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
