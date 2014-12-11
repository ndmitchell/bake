{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Server.Brains(
    brains, Neuron(..)
    ) where

import Development.Bake.Core.Message
import Development.Bake.Core.Type
import Development.Bake.Server.Type
import Development.Bake.Server.Query
import Control.DeepSeq
import Data.Maybe
import Data.Monoid
import Data.List.Extra
import Data.Tuple.Extra
import General.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map


data Neuron
    = Sleep -- nothing useful to do
    | Task Question
    | Update (State, [Patch])-- update to the target state
    | Reject Patch (Maybe Test) -- reject this patch
      deriving (Show,Eq)

instance NFData Neuron where
    rnf Sleep = ()
    rnf (Task x) = rnf x
    rnf (Update x) = rnf x
    rnf (Reject x y) = rnf x `seq` rnf y

-- Given a ping from a client, figure out what work we can get them to do, if anything
brains :: (Test -> TestInfo Test) -> Server -> Ping -> Neuron
brains info server@Server{..} Ping{..}
    | (i,_):_ <- filter (isBlessed . snd) pinfo, i /= 0 || null (snd target)
        = if i == 0 then Sleep else Update $ second (take i) target
    | Just (i, test) <- findBlame pinfo = Reject (snd target !! (i-1)) test
    | (c,t):_ <- filter (uncurry suitableTest) $ if null failures then todoPass else todoFail
        = Task $ Question c t (threadsForTest t) pClient
    | otherwise = Sleep
    where
        prep = prepare server
        pinfo = patchInfo prep

        failures = targetFailures server

        -- all the tests, sorted so those which have been done least are first
        todoPass :: [((State, [Patch]), Maybe Test)]
        todoPass
            | (i,PatchInfo{..}):_ <- pinfo, i == length (snd target), xs@(_:_) <- Set.toList patchTodo =
                let orderAsked t = if t `Set.member` patchSuccess || t `Set.member` patchFailure then 0
                                   else if t `Set.member` patchAsked then 1 else 2
                    orderPriority = maybe 0 (negate . testPriority . info)
                    orderRarity t = head $ [i | (i,PatchInfo{..}) <- pinfo, t `Set.member` patchSuccess] ++ [-1]
                in map (target,) $ sortOn (\x -> (orderAsked x, orderPriority x, orderRarity x)) xs
            | otherwise = [(target, Nothing)]


        todoFail = unasked [((fst target, init ps), t) | (t, ps@(_:_)) <- failures, t <- dependencies [t]]
        unasked xs = no ++ yes
            where started = map ((qCandidate &&& qTest) . fst) $ asked server []
                  (yes, no) = partition (`elem` started) xs

        dependencies = transitiveClosure $ \t -> case t of
            Nothing -> []
            Just t -> Nothing : map Just (testRequire $ info t)

        -- how many threads does this test require
        threadsForTest = maybe 1 (fromMaybe pMaxThreads . testThreads . info)

        -- can this candidate start running this test
        -- use candidateExact since we must have prepared in this directory for it to work
        suitableTest c t
            | threadsForTest t > pNowThreads = False -- not enough threads
        suitableTest c Nothing
            | null $ asked server [self', test' Nothing, candidateExact' c] -- I am not already running it
            = True
        suitableTest c t@(Just tt)
            | clientTests:_ <- map (fst . aTestsSuitable . snd) $ answered server [self', success', test' Nothing, candidateExact' c]
            , tt `elem` clientTests -- it is one of the tests this client is suitable for
            , null $ asked server [test' t, self', candidateExact' c] -- I am not running it or have run it
            , clientDone <- map (qTest . fst) $ answered server [success', self', candidateExact' c]
            , all (`elem` clientDone) $ map Just $ testRequire $ info tt -- I have done all the dependencies
            = True
        suitableTest _ _ = False

        self' = client' pClient


----------------------------------------------------------------

-- | From the history, find those which are the current target state, plus some prefix of patches
--   The Int is how many patches.
prepare :: Server -> [(Int, Question, Maybe Answer)]
prepare server@Server{..} =
    [ (length p, q, a)
    | (_,q,a) <- history
    , Just p <- [translate server (fst target) $ qCandidate q]
    , p `isPrefixOf` snd target]


isBlessed :: PatchInfo -> Bool
isBlessed PatchInfo{patchTodo=t, patchSuccess=s} = not (Set.null t) && Set.size t == Set.size s


findBlame :: [(Int,PatchInfo)] -> Maybe (Int, Maybe Test)
findBlame ((i,a):(j,b):_)
    | i - 1 == j, not $ Set.null $ patchTodo b, bad:_ <- Set.toList $ blame a b = Just (i, bad)
    where
        blame PatchInfo{patchFailure=failure} PatchInfo{patchTodo=todo, patchSuccess=success} =
            (failure `Set.intersection` success) `Set.union` -- failed this time, success the time before
            (failure `Set.difference` todo) -- a new test that failed
findBlame ((i,a):_) -- assume the state is good, even if you don't have evidence
    | i == 1, bad:_ <- Set.toList $ patchFailure a = Just (i, bad)
findBlame (_:xs) = findBlame xs
findBlame [] = Nothing


data PatchInfo = PatchInfo
    {patchTodo :: Set.Set (Maybe Test) -- empty means we haven't run it yet, or we did and it failed
    ,patchSuccess :: Set.Set (Maybe Test)
    ,patchFailure :: Set.Set (Maybe Test)
    ,patchAsked :: Set.Set (Maybe Test)
    } deriving Show

instance Monoid PatchInfo where
    mempty = PatchInfo Set.empty Set.empty Set.empty Set.empty
    mappend (PatchInfo x1 x2 x3 x4) (PatchInfo y1 y2 y3 y4) =
        PatchInfo (if Set.null x1 then y1 else x1) (x2 `Set.union` y2) (x3 `Set.union` y3) (x4 `Set.union` y4)

-- | Return patch info, sorted from highest number of patches to lowest
patchInfo :: [(Int, Question, Maybe Answer)] -> [(Int,PatchInfo)]
patchInfo = Map.toDescList . Map.fromListWith mappend . map (fst3 &&& f)
    where
        f (_, Question{qTest=Nothing}, Just Answer{aSuccess=True, aTestsSuitable=(a,b)})
            = mempty{patchTodo = Set.fromList $ Nothing : map Just (a ++ b), patchSuccess=Set.singleton Nothing}
        f (_, Question{qTest=t}, Just Answer{aSuccess=b})
            = if b then mempty{patchSuccess=Set.singleton t} else mempty{patchFailure=Set.singleton t}
        f (_, Question{qTest=t}, Nothing)
            = mempty{patchAsked=Set.singleton t}
