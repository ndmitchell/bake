{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

-- | Define a continuous integration system.
module Development.Bake.Core.Type(
    Host, Port,
    Stringy(..), readShowStringy,
    Oven(..), TestInfo(..), defaultOven, ovenTest, ovenNotifyStdout,
    threads, threadsAll, require, run, suitable, priority,
    State(..), Patch(..), Test(..), Client(..), concrete, validate,
    Author
    ) where

import General.Extra
import Control.Monad.Extra
import Control.DeepSeq
import Data.Monoid
import Data.Aeson
import Data.Hashable
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
--   'ovenPatchExtra' will always be called from @bake-extra-/hash/@;
--   'ovenPrepare', 'run' and 'ovenUpdate' will always be called from @bake-test-/hash/@.
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
    ,ovenNotify :: [Author] -> String -> IO ()
        -- ^ Tell an author some information contained in the string (usually an email)
    ,ovenPatchExtra :: state -> Maybe patch -> IO (String, String)
        -- ^ Extra information about a patch, a single line (HTML span),
        --   and a longer chunk (HTML block)
    ,ovenServer :: (Host, Port)
        -- ^ Default server to use
    ,ovenSupersede :: patch -> patch -> Bool
        -- ^ Given two patches (first on submitted first) is the first now redundant
    ,ovenStringyState :: Stringy state
    ,ovenStringyPatch :: Stringy patch
    ,ovenStringyTest :: Stringy test
    }

-- | Given a 'Stringy' for @test@, and a function that when run on a code base
--   returns the list of tests that need running, and a function to populate
--   a 'TestInfo', modify the 'Oven' with a test type.
ovenTest :: Stringy test -> IO [test] -> (test -> TestInfo test)
         -> Oven state patch () -> Oven state patch test
ovenTest stringy prepare info o = o{ovenStringyTest=stringy, ovenPrepare= \_ _ -> prepare, ovenTestInfo=info}

-- | Produce notifications on 'stdout' when users should be notified about success/failure.
ovenNotifyStdout :: Oven state patch test -> Oven state patch test
ovenNotifyStdout o = o{ovenNotify = \a s -> f a s >> ovenNotify o a s}
    where f a s = putBlock "Email" ["To: " ++ commas a, s]

-- | A type representing a translation between a value and a string, which can be
--   produced by 'readShowStringy' if the type has both 'Read' and 'Show' instances.
--   The functions 'stringyTo' and 'stringyFrom' should be inverses of each other.
--   The function 'stringyPretty' shows a value in a way suitable for humans, and can
--   discard uninteresting information.
data Stringy s = Stringy
    {stringyTo :: s -> String
    ,stringyFrom :: String -> s
    ,stringyPretty :: s -> String
    }

-- | Produce a 'Stringy' for a type with 'Read' and 'Show'.
readShowStringy :: (Show s, Read s) => Stringy s
readShowStringy = Stringy show read show

-- | The default oven, which doesn't do anything interesting. Usually the starting point.
defaultOven :: Oven () () ()
defaultOven = Oven
    {ovenInit = return ()
    ,ovenUpdate = \_ _ -> return ()
    ,ovenNotify = \_ _ -> return ()
    ,ovenPrepare = \_ _ -> return []
    ,ovenTestInfo = \_ -> mempty
    ,ovenPatchExtra = \_ _ -> return ("","")
    ,ovenServer = ("127.0.0.1",80)
    ,ovenSupersede = \_ _ -> False
    ,ovenStringyState = readShowStringy
    ,ovenStringyPatch = readShowStringy
    ,ovenStringyTest = readShowStringy
    }

-- | Information about a test.
data TestInfo test = TestInfo
    {testThreads :: Maybe Int -- number of threads, defaults to 1, Nothing for use all
    ,testAction :: IO ()
    ,testSuitable :: IO Bool -- can this test be run on this machine (e.g. Linux only tests)
    ,testRequire :: [test]
    ,testPriority :: Int
    }

instance Functor TestInfo where
    fmap f t = t{testRequire = map f $ testRequire t}

instance Monoid (TestInfo test) where
    mempty = TestInfo (Just 1) (return ()) (return True) [] 0
    mappend (TestInfo x1 x2 x3 x4 x5) (TestInfo y1 y2 y3 y4 y5) =
        TestInfo (liftM2 (+) x1 y1) (x2 >> y2) (x3 &&^ y3) (x4 ++ y4) (x5 + y5)

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
require :: [test] -> TestInfo test -> TestInfo test
require xs t = t{testRequire=testRequire t++xs}

-- | The action associated with a @test@.
run :: IO () -> TestInfo test
run act = mempty{testAction=act}

-- | Set the priority of a test, those with higher priority are run first.
--   Tests have a default priority of 0.
priority :: Int -> TestInfo test -> TestInfo test
priority p t = t{testPriority = p + testPriority t}

-- | Is a particular client capable of running a test.
--   Usually an OS check.
suitable :: IO Bool -> TestInfo test -> TestInfo test
suitable query t = t{testSuitable = query &&^ testSuitable t}


newtype State = State {fromState :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData)
newtype Patch = Patch {fromPatch :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData)
newtype Test = Test {fromTest :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData)
newtype Client = Client {fromClient :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON,Hashable,NFData)

concrete :: Oven state patch test -> Oven State Patch Test
concrete o@Oven{..} = o
    {ovenInit = fmap restate ovenInit
    ,ovenUpdate = \s ps -> fmap restate $ ovenUpdate (unstate s) (map unpatch ps)
    ,ovenPrepare = \s ps -> fmap (map retest) $ ovenPrepare (unstate s) (map unpatch ps)
    ,ovenTestInfo = fmap retest . ovenTestInfo . untest
    ,ovenPatchExtra = \s p -> ovenPatchExtra (unstate s) (fmap unpatch p)
    ,ovenSupersede = \p1 p2 -> ovenSupersede (unpatch p1) (unpatch p2) 
    ,ovenStringyState = state
    ,ovenStringyPatch = patch
    ,ovenStringyTest  = test
    }
    where
        (patch,unpatch,_      ) = f Patch fromPatch ovenStringyPatch
        (state,unstate,restate) = f State fromState ovenStringyState
        (test ,untest ,retest ) = f Test  fromTest  ovenStringyTest

        f :: (String -> s) -> (s -> String) -> Stringy o -> (Stringy s, s -> o, o -> s)
        f inj proj Stringy{..} =
            (Stringy proj inj (stringyPretty . stringyFrom . proj)
            ,stringyFrom . proj
            ,inj . stringyTo)

validate :: Oven state patch test -> Oven state patch test
validate o@Oven{..} = o
    {ovenStringyState = f ovenStringyState
    ,ovenStringyPatch = f ovenStringyPatch
    ,ovenStringyTest = f ovenStringyTest
    }
    where
        f :: Stringy a -> Stringy a
        f s@Stringy{..} = s
            {stringyTo = check . stringyTo
            ,stringyFrom = stringyFrom . check
            }
            where check s | null s = error "Problem with stringyTo/stringyFrom, generated blank string"
                          | s == stringyTo (stringyFrom s) = s
                          | otherwise = error $ "Problem with stringyTo/stringyFrom on " ++ show s
