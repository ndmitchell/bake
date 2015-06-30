{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Define a continuous integration system.
module Development.Bake.Core.Type(
    Host, Port,
    Stringy(..),
    Oven(..), TestInfo(..), defaultOven, ovenTest,
    ovenNotifyAdd, ovenNotifyStdout,
    threads, threadsAll, depend, run, require, priority,
    State, toState, fromState,
    Patch, toPatch, fromPatch,
    Test, toTest, fromTest,
    Client, toClient, fromClient,
    Point,
    concrete, Prettys(..),
    Author
    ) where

import General.Extra
import Control.Monad.Extra
import Control.DeepSeq
import Data.Monoid
import Data.Aeson
import Data.Hashable
import Data.Typeable
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import General.Database
import Data.List.Extra
import Prelude


type Author = String

type Host = String

type Port = Int

-- | The central type defining a continuous integration system.
--   Usually constructed with 'defaultOven' then filled out with other
--   'Oven' modifiers such as 'ovenGit' and 'ovenTest'.
--
--   The types are: @state@ is the base state of the system (think HEAD on the master branch);
--   @patch@ is a change that is proposed (think a diff); @test@ is the type of tests that
--   are run.
--
--   All IO operations will be called in a direct subdirectory of the directory you start
--   'bake' from. In particular:
--   'ovenInit' will always be called single-threaded from @bake-init@;
--   'ovenUpdate' will always be called single-threaded from @bake-update-/hash/@;
--   'ovenPatchExtra' will always be called from @bake-extra-/hash/@;
--   'ovenPrepare' and 'run' will always be called from @bake-test-/hash/@.
data Oven state patch test = Oven
    {ovenInit :: IO state
        -- ^ Get an initial state
    ,ovenUpdate :: state -> [patch] -> IO state
        -- ^ Given a state, and a set of candiates that have passed,
        --   merge to create a new state
    ,ovenPrepare :: state -> [patch] -> IO [test]
        -- ^ Prepare a candidate to be run, produces the tests that must pass
    ,ovenTestInfo :: test -> TestInfo test
        -- ^ Produce information about a test
    ,ovenNotify :: Author -> String -> String -> IO ()
        -- ^ Tell an author some information. The first 'String' is a subject line, the second an HTML fragment.
    ,ovenPatchExtra :: state -> Maybe patch -> IO (String, String)
        -- ^ Extra information about a patch, a single line (HTML span),
        --   and a longer chunk (HTML block)
    ,ovenServer :: (Host, Port)
        -- ^ Default server to use
    ,ovenSupersede :: patch -> patch -> Bool
        -- ^ Given two patches (first on submitted first) is the first now redundant
    }

-- | Given a 'Stringy' for @test@, and a function that when run on a code base
--   returns the list of tests that need running, and a function to populate
--   a 'TestInfo', modify the 'Oven' with a test type.
ovenTest :: IO [test] -> (test -> TestInfo test)
         -> Oven state patch () -> Oven state patch test
ovenTest prepare info o = o{ovenPrepare= \_ _ -> prepare, ovenTestInfo=info}

-- | Add an additional notification to the list.
ovenNotifyAdd :: (Author -> String -> String -> IO ()) -> Oven state patch test -> Oven state patch test
ovenNotifyAdd f o = o{ovenNotify = \a s b -> f a s b >> ovenNotify o a s b}

-- | Produce notifications on 'stdout' when users should be notified about success/failure.
ovenNotifyStdout :: Oven state patch test -> Oven state patch test
ovenNotifyStdout = ovenNotifyAdd $ \author subject body ->
    putBlock "Email" ["To: " ++ author, "Subject: " ++ subject, body]

-- | A type representing a translation between a value and a string, which can be
--   produced by 'readShowStringy' if the type has both 'Read' and 'Show' instances.
--   The functions 'stringyTo' and 'stringyFrom' should be inverses of each other.
--   The function 'stringyPretty' shows a value in a way suitable for humans, and can
--   discard uninteresting information.
class Stringy s where
    stringyTo :: s -> String
    stringyFrom :: String -> s
    stringyPretty :: s -> String
    stringyPretty = stringyTo

instance Stringy () where
    stringyTo () = "_"
    stringyFrom "_" = ()
    stringyFrom x = error $ "Invalid stringyFrom on (), expected \"_\", got " ++ show x

instance Stringy String where
    stringyTo = id
    stringyFrom = id
    stringyPretty x
        | (pre,sha) <- spanEnd (`elem` "0123456789abcdef") x
        , length sha >= 32 -- git is 40
        = pre ++ take 7 sha
    stringyPretty x = x


-- | The default oven, which doesn't do anything interesting. Usually the starting point.
defaultOven :: Oven () () ()
defaultOven = Oven
    {ovenInit = return ()
    ,ovenUpdate = \_ _ -> return ()
    ,ovenNotify = \_ _ _ -> return ()
    ,ovenPrepare = \_ _ -> return []
    ,ovenTestInfo = \_ -> mempty
    ,ovenPatchExtra = \_ _ -> return ("","")
    ,ovenServer = ("127.0.0.1",80)
    ,ovenSupersede = \_ _ -> False
    }

-- | Information about a test.
data TestInfo test = TestInfo
    {testThreads :: Maybe Int -- number of threads, defaults to 1, Nothing for use all
    ,testAction :: IO ()
    ,testRequire :: [String] -- attributes that are required
    ,testDepend :: [test]
    ,testPriority :: Int
    }

instance Functor TestInfo where
    fmap f t = t{testDepend = map f $ testDepend t}

instance Monoid (TestInfo test) where
    mempty = TestInfo (Just 1) (return ()) [] [] 0
    mappend (TestInfo x1 x2 x3 x4 x5) (TestInfo y1 y2 y3 y4 y5) =
        TestInfo (liftM2 (+) x1 y1) (x2 >> y2) (x3 ++ y3) (x4 ++ y4) (x5 + y5)

-- | Change the number of threads a test requires, defaults to 1.
threads :: Int -> TestInfo test -> TestInfo test
threads j t = t{testThreads=Just j}

-- | Record that a test requires all available threads on a machine,
--   typically used for the build step.
--   Use 'getNumCapabilities' to find out how many threads you were allocated.
threadsAll :: TestInfo test -> TestInfo test
threadsAll t = t{testThreads=Nothing}


-- | Require the following tests have been evaluated on this machine
--   before this test is run. Typically used to require compilation
--   before running most tests.
depend :: [test] -> TestInfo test -> TestInfo test
depend xs t = t{testDepend=testDepend t++xs}

-- | The action associated with a @test@.
run :: IO () -> TestInfo test
run act = mempty{testAction=act}

-- | Set the priority of a test, those with higher priority are run first.
--   Tests have a default priority of 0.
priority :: Int -> TestInfo test -> TestInfo test
priority p t = t{testPriority = p + testPriority t}

-- | Is a particular client capable of running a test.
--   Usually an OS check. To run a test must have all its requirements met.
--   Clients can satisfy a requirement by passing @--provide=...@ on the command line.
require :: [String] -> TestInfo test -> TestInfo test
require xs t = t{testRequire = xs ++ testRequire t}


newtype State = State {fromState :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData,Typeable,FromField,ToField,TypeField,Stringy)
newtype Patch = Patch {fromPatch :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData,Typeable,FromField,ToField,TypeField,Stringy)
newtype Test = Test {fromTest :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData,Typeable,FromField,ToField,TypeField,Stringy)
newtype Client = Client {fromClient :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData,Typeable,FromField,ToField,TypeField)

toState :: String -> State; toState = State
toPatch :: String -> Patch; toPatch = Patch
toTest :: String -> Test; toTest = Test
toClient :: String -> Client; toClient = Client

type Point = (State, [Patch])

data Prettys = Prettys
    {prettyState :: State -> String
    ,prettyPatch :: Patch -> String
    ,prettyTest  :: Test  -> String
    }

concrete :: (Stringy state, Stringy patch, Stringy test) => Oven state patch test -> (Prettys, Oven State Patch Test)
concrete o@Oven{..} = (Prettys prestate prepatch pretest, o
    {ovenInit = fmap restate ovenInit
    ,ovenUpdate = \s ps -> fmap restate $ ovenUpdate (unstate s) (map unpatch ps)
    ,ovenPrepare = \s ps -> fmap (map retest) $ ovenPrepare (unstate s) (map unpatch ps)
    ,ovenTestInfo = fmap retest . ovenTestInfo . untest
    ,ovenPatchExtra = \s p -> ovenPatchExtra (unstate s) (fmap unpatch p)
    ,ovenSupersede = \p1 p2 -> ovenSupersede (unpatch p1) (unpatch p2) 
    })
    where
        (unstate,restate,prestate) = f State fromState
        (unpatch,_      ,prepatch) = f Patch fromPatch
        (untest ,retest ,pretest ) = f Test  fromTest

        f :: forall o s . Stringy o => (String -> s) -> (s -> String) -> (s -> o, o -> s, s -> String)
        f inj proj =
            (check . stringyFrom . proj
            ,inj . stringyTo . check
            ,stringyPretty . flip asTypeOf (undefined :: o) . check . stringyFrom . proj)

        check :: forall o . Stringy o => o -> o
        check s | null $ stringyTo s = error "Problem with stringyTo/stringyFrom, generated blank string"
                | stringyTo s == stringyTo (stringyFrom (stringyTo s) :: o) = s
                | otherwise = error $ "Problem with stringyTo/stringyFrom on " ++ stringyTo s
