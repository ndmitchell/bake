{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Type(
    Host, Port,
    Stringy(..), readShowStringy,
    Candidate(..), Oven(..), TestInfo(..), defaultOven, ovenTest,
    threads, threadsAll, before, beforeClear, run, suitable,
    State(..), Patch(..), Test(..), concrete,
    Client(..), newClient,
    Author
    ) where

import Development.Bake.Util
import Control.Monad
import Data.Monoid


type Author = String

type Host = String

type Port = Int

data Candidate state patch = Candidate state [patch] deriving (Show,Eq)

data Oven state patch test = Oven
    {ovenUpdateState :: Maybe (Candidate state patch) -> IO state
        -- ^ Given a state, and a set of candiates that have passed,
        --   merge to create a new state.
    ,ovenRunTest :: Candidate state patch -> Maybe test -> TestInfo test
        -- ^ Produce information about a test
    ,ovenNotify :: Author -> String -> IO ()
        -- ^ Tell an author some information contained in the string (usually an email)
    ,ovenDefaultServer :: (Host, Port)
        -- ^ Default server to use
    ,ovenStringyState :: Stringy state
    ,ovenStringyPatch :: Stringy patch
    ,ovenStringyTest :: Stringy test
    }

ovenTest :: Stringy test -> (Candidate state patch -> Maybe test -> TestInfo test) -> Oven state patch () -> Oven state patch test
ovenTest a b o = o{ovenRunTest=b, ovenStringyTest=a}

data Stringy s = Stringy
    {stringyTo :: s -> String
    ,stringyFrom :: String -> s
    ,stringyPretty :: s -> String
    ,stringyExtra :: s -> IO String
    }

readShowStringy :: (Show s, Read s) => Stringy s
readShowStringy = Stringy show read show (const $ return "")

defaultOven :: Oven () () ()
defaultOven = Oven
    {ovenUpdateState = \_ -> return ()
    ,ovenNotify = \_ _ -> return ()
    ,ovenRunTest = \_ _ -> run $ return []
    ,ovenDefaultServer = ("",0)
    ,ovenStringyState = readShowStringy
    ,ovenStringyPatch = readShowStringy
    ,ovenStringyTest = readShowStringy
    }

data TestInfo test = TestInfo
    {testThreads :: Maybe Int -- number of threads, defaults to 1, Nothing for use all
    ,testAction :: IO [test]
    ,testSuitable :: IO Bool -- can this test be run on this machine (e.g. Linux only tests)
    ,testBefore :: [test]
    ,testBeforeAuto :: Bool
    }

instance Functor TestInfo where
    fmap f t = t{testBefore = map f $ testBefore t
                ,testAction = fmap (map f) $ testAction t
                }

instance Monoid (TestInfo test) where
    mempty = TestInfo (Just 1) (return []) (return True) [] True
    mappend (TestInfo x1 x2 x3 x4 x5) (TestInfo y1 y2 y3 y4 y5) =
        TestInfo (liftM2 (+) x1 y1) (liftM2 (++) x2 y2) (x3 &&^ y3) (x4 ++ y4) (x5 || y5)

threads :: Int -> TestInfo test -> TestInfo test
threads j t = t{testThreads=Just j}

threadsAll :: TestInfo test -> TestInfo test
threadsAll t = t{testThreads=Nothing}

before :: [test] -> TestInfo test -> TestInfo test
before xs t = t{testBefore=testBefore t++xs}

beforeClear :: TestInfo test -> TestInfo test
beforeClear t = t{testBefore=[], testBeforeAuto=False}

run :: IO [test] -> TestInfo test
run act = mempty{testAction=act}

suitable :: IO Bool -> TestInfo test -> TestInfo test
suitable query t = t{testSuitable = query &&^ testSuitable t}


newtype State = State {fromState :: String} deriving (Show,Eq)
newtype Patch = Patch {fromPatch :: String} deriving (Show,Eq)
newtype Test = Test {fromTest :: String} deriving (Show,Eq)

concrete :: Oven state patch test -> Oven State Patch Test
concrete o@Oven{..} = o
    {ovenUpdateState = \mc ->
        fmap (State . stringyTo ovenStringyState) $ ovenUpdateState $ fmap downCandidate mc
    ,ovenRunTest = \c mt -> error "concrete ovenRunTest"
    ,ovenStringyState = liftStringy ovenStringyState
    ,ovenStringyPatch = liftStringy ovenStringyPatch
    ,ovenStringyTest = liftStringy ovenStringyTest
    }
    where
        liftStringy = error "conrete liftStringy"

        downCandidate (Candidate s ps) = Candidate
            (stringyFrom ovenStringyState $ fromState s) $
            map (stringyFrom ovenStringyPatch . fromPatch) ps


newtype Client = Client String deriving (Show,Eq)

newClient :: IO Client
newClient = error "newCookie"

