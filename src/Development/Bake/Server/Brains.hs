{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Development.Bake.Server.Brains(
    brains, Neuron(..)
    ) where

import Development.Bake.Core.Message
import Development.Bake.Core.Type
import Development.Bake.Server.Type
import Development.Bake.Server.Query(asked, test', candidateExact', answered, success', client')
import Control.DeepSeq
import Data.Maybe
import Data.Monoid
import Data.List.Extra
import Data.Tuple.Extra
import General.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude


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
    | null $ snd target, isBlessed target = Sleep
    | pt:_ <- filter isBlessed points = Update pt
    | (pt,t):_ <- [(pt,t) | pt <- points, Just t <- [isRejected pt]] = Reject (last $ snd pt) t
    | (c,t):_ <- filter (uncurry suitableTest) $ if isNothing failure then todoPass else todoFail
        = Task $ Question c t (threadsForTest t) pClient
    | otherwise = Sleep
    where
        points = map (fst target,) $ reverse $ tail $ inits $ snd target

        isBlessed (newPoint server -> pt)
            | Just PointInfo{..} <- Map.lookup pt pointInfo
            , Just t <- poTests
            = Set.size t + 1 == Map.size poPass -- +1 for the Nothing test
        isBlessed _ = False

        isRejected (s, [p]) -- weird special case, assuming the zero state is blessed
            | Just PointInfo{..} <- Map.lookup (newPoint server (s,[p])) pointInfo
            = listToMaybe $ Map.keys poFail
        isRejected (newPoint server -> pt)
            | Just PointInfo{..} <- Map.lookup pt pointInfo
            = listToMaybe $ Set.toList poReject
        isRejected _ = Nothing


        prep = prepare server
        pinfo = patchInfo2 prep

        -- pick a single failure to chase down, don't look at any others til that is eliminated
        failure = listToMaybe [(i,x) | (i,PatchInfo2{..}) <- reverse pinfo, i /= 0, x:_ <- [Set.toList patchFailure]]

        -- all the tests, sorted so those which have been done least are first
        todoPass
            | (i,PatchInfo2{..}):_ <- pinfo, i == length (snd target), xs@(_:_) <- Set.toList patchTodo =
                let orderAsked t = if t `Set.member` patchSuccess || t `Set.member` patchFailure then 0
                                   else if t `Set.member` patchAsked then 1 else 2
                    orderPriority = maybe 0 (negate . testPriority . info)
                    orderRarity t = head $ [i | (i,PatchInfo2{..}) <- pinfo, t `Set.member` patchSuccess] ++ [-1]
                in map (target,) $ sortOn (\x -> (orderAsked x, orderPriority x, orderRarity x)) xs
            | otherwise = [(target, Nothing)]

        -- all the tests that are dependencies
        todoFail | Just (bad,t) <- failure =
            let good = maybe 0 fst $ find (Set.member t . patchSuccess . snd) $ dropWhile ((>= bad) . fst) pinfo
                mid = good + ((bad - good) `div` 2)
            in [(second (take mid) target, t) | t <- dependencies [t]]

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


data PatchInfo2 = PatchInfo2
    {patchTodo :: Set.Set (Maybe Test) -- empty means we haven't run it yet, or we did and it failed
    ,patchSuccess :: Set.Set (Maybe Test)
    ,patchFailure :: Set.Set (Maybe Test)
    ,patchAsked :: Set.Set (Maybe Test)
    } deriving Show

instance Monoid PatchInfo2 where
    mempty = PatchInfo2 Set.empty Set.empty Set.empty Set.empty
    mappend (PatchInfo2 x1 x2 x3 x4) (PatchInfo2 y1 y2 y3 y4) =
        PatchInfo2 (if Set.null x1 then y1 else x1) (x2 `Set.union` y2) (x3 `Set.union` y3) (x4 `Set.union` y4)

-- | Return patch info, sorted from highest number of patches to lowest
patchInfo2 :: [(Int, Question, Maybe Answer)] -> [(Int,PatchInfo2)]
patchInfo2 = Map.toDescList . Map.fromListWith mappend . map (fst3 &&& f)
    where
        f (_, Question{qTest=Nothing}, Just Answer{aSuccess=True, aTestsSuitable=(a,b)})
            = mempty{patchTodo = Set.fromList $ Nothing : map Just (a ++ b), patchSuccess=Set.singleton Nothing}
        f (_, Question{qTest=t}, Just Answer{aSuccess=b})
            = if b then mempty{patchSuccess=Set.singleton t} else mempty{patchFailure=Set.singleton t}
        f (_, Question{qTest=t}, Nothing)
            = mempty{patchAsked=Set.singleton t}
