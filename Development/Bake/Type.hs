{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

-- | Define a continuous integration system.
module Development.Bake.Type(
    Host, Port,
    Stringy(..), readShowStringy,
    Candidate(..), Oven(..), TestInfo(..), defaultOven, ovenTest, ovenNotifyStdout,
    threads, threadsAll, require, run, suitable,
    State(..), Patch(..), Test(..), Client(..), concrete,
    Author
    ) where

import Control.Monad.Extra
import Data.Monoid
import Data.Aeson
import Data.List


type Author = String

type Host = String

type Port = Int

data Candidate state patch = Candidate {candidateState :: state, candidatePatches :: [patch]} deriving (Show,Eq,Ord)

data Oven state patch test = Oven
    {ovenUpdateState :: Maybe (Candidate state patch) -> IO state
        -- ^ Given a state, and a set of candiates that have passed,
        --   merge to create a new state.
    ,ovenPrepare :: Candidate state patch -> IO [test]
        -- ^ Prepare a candidate to be run, produces the tests that must pass
    ,ovenTestInfo :: test -> TestInfo test
        -- ^ Produce information about a test
    ,ovenNotify :: [Author] -> String -> IO ()
        -- ^ Tell an author some information contained in the string (usually an email)
    ,ovenServer :: (Host, Port)
        -- ^ Default server to use
    ,ovenStringyState :: Stringy state
    ,ovenStringyPatch :: Stringy patch
    ,ovenStringyTest :: Stringy test
    }

ovenTest :: Stringy test -> IO [test] -> (test -> TestInfo test)
         -> Oven state patch () -> Oven state patch test
ovenTest stringy prepare info o = o{ovenStringyTest=stringy, ovenPrepare=const prepare, ovenTestInfo=info}

ovenNotifyStdout :: Oven state patch test -> Oven state patch test
ovenNotifyStdout o = o{ovenNotify = \a s -> f a s >> ovenNotify o a s}
    where f a s = putStr $ unlines
                    [replicate 70 '-'
                    ,"To: " ++ intercalate ", " a
                    ,s
                    ,replicate 70 '-'
                    ]

data Stringy s = Stringy
    {stringyTo :: s -> String
    ,stringyFrom :: String -> s
    ,stringyPretty :: s -> String
    ,stringyExtra :: s -> IO String
    }

readShowStringy :: (Show s, Read s) => Stringy s
readShowStringy = isoStringy read show

isoStringy :: (String -> a) -> (a -> String) -> Stringy a
isoStringy read show = Stringy show read show (const $ return "")

defaultOven :: Oven () () ()
defaultOven = Oven
    {ovenUpdateState = \_ -> return ()
    ,ovenNotify = \_ _ -> return ()
    ,ovenPrepare = \_ -> return []
    ,ovenTestInfo = \_ -> mempty
    ,ovenServer = ("127.0.0.1",80)
    ,ovenStringyState = readShowStringy
    ,ovenStringyPatch = readShowStringy
    ,ovenStringyTest = readShowStringy
    }

data TestInfo test = TestInfo
    {testThreads :: Maybe Int -- number of threads, defaults to 1, Nothing for use all
    ,testAction :: IO ()
    ,testSuitable :: IO Bool -- can this test be run on this machine (e.g. Linux only tests)
    ,testRequire :: [test]
    }

instance Functor TestInfo where
    fmap f t = t{testRequire = map f $ testRequire t}

instance Monoid (TestInfo test) where
    mempty = TestInfo (Just 1) (return ()) (return True) []
    mappend (TestInfo x1 x2 x3 x4) (TestInfo y1 y2 y3 y4) =
        TestInfo (liftM2 (+) x1 y1) (x2 >> y2) (x3 &&^ y3) (x4 ++ y4)

threads :: Int -> TestInfo test -> TestInfo test
threads j t = t{testThreads=Just j}

threadsAll :: TestInfo test -> TestInfo test
threadsAll t = t{testThreads=Nothing}

require :: [test] -> TestInfo test -> TestInfo test
require xs t = t{testRequire=testRequire t++xs}

run :: IO () -> TestInfo test
run act = mempty{testAction=act}

suitable :: IO Bool -> TestInfo test -> TestInfo test
suitable query t = t{testSuitable = query &&^ testSuitable t}


newtype State = State {fromState :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON)
newtype Patch = Patch {fromPatch :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON)
newtype Test = Test {fromTest :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON)
newtype Client = Client {fromClient :: String} deriving (Show,Eq,Ord,ToJSON,FromJSON)

concrete :: Oven state patch test -> Oven State Patch Test
concrete o@Oven{..} = o
    {ovenUpdateState =
        fmap (State . stringyTo ovenStringyState) . ovenUpdateState . fmap stringyFromCandidate
    ,ovenPrepare = fmap (map (Test . stringyTo ovenStringyTest)) . ovenPrepare . stringyFromCandidate
    ,ovenTestInfo = fmap (Test . stringyTo ovenStringyTest) . ovenTestInfo . stringyFrom ovenStringyTest . fromTest
    ,ovenStringyState = f State fromState ovenStringyState
    ,ovenStringyPatch = f Patch fromPatch ovenStringyPatch
    ,ovenStringyTest  = f Test  fromTest  ovenStringyTest
    }
    where
        stringyFromCandidate (Candidate s ps) = Candidate
            (stringyFrom ovenStringyState $ fromState s) $
            map (stringyFrom ovenStringyPatch . fromPatch) ps

        f :: (String -> c) -> (c -> String) -> Stringy a -> Stringy c
        f inj proj Stringy{..} = Stringy
            {stringyTo = proj
            ,stringyFrom = inj
            ,stringyPretty = stringyPretty . stringyFrom . proj
            ,stringyExtra = stringyExtra . stringyFrom . proj
            }
