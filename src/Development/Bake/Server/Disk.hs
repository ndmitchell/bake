
-- Stuff on disk on the server
module Development.Bake.Server.Disk(
    Cache, newCache,
    Q(..), load, save
    ) where

type Point = (State, [Patch])

data Q result where
    StateExtraShort :: State -> Q Text
    StateExtraLong :: State -> Q Text

    PatchExtraShort :: Patch -> Q Text
    PatchExtraLong :: Patch -> Q Text
    PatchAuthors :: Patch -> Q [String]
    PatchSubmitted :: Patch -> Q UTCTime
    PatchRejected :: Patch -> Q [(UTCTime, State, [Patch], Maybe Test)]
    PatchSuperseded :: Patch -> Q (Maybe UTCTime)
    PatchPlausible :: Patch -> Q (Maybe UTCTime)
    PatchMerged :: Patch -> Q (Maybe UTCTime)
    PatchRejected :: Patch -> Q (Maybe UTCTime)

    PointTests :: Point -> Q (Maybe [Test])
    PointRunSucceed :: Point -> Maybe Test -> Q Bool -- True only if all tests succeed
    PointRunDuration :: Point -> Maybe Test -> Q (UTCTime, Duration)
    PointRunClient :: Point -> Maybe Test -> Q (UTCTime, Client)

    SkipAuthor :: Patch -> Q [String]

    ListPatch :: V [Patch]
    ListSkip :: V [Test]

    -- advanced queries that get updated
    ListPatchAlive :: V [Patch] -- ^ Patches that are not merged/superseded/rejected
    ListPatchSubmitted :: Date -> V [Patch] -- ^ Submitted on a date
    ListPatchActive :: Date -> V [Patch] -- ^ Active on a date


-- caches with cache invalidation
load :: Cache -> V result -> IO result


save :: Cache -> V result -> result -> IO ()
