{-# LANGUAGE Rank2Types #-}

-- | Define a continuous integration system.
module Development.Bake.Type(
    Host, Port,
    Stringy(..), readShowStringy,
    Candidate(..), Oven(..), TestInfo(..), defaultOven, ovenTest,
    threads, threadsAll, before, beforeClear, run, require,
    State(..), Patch(..), Test(..), concrete,
    Author
    ) where

type Author = String

type Host = String

type Port = Int

data Candidate state patch = Candidate state [patch] deriving Show

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
ovenTest = undefined

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
    ,testBefore :: [test]
    ,testBeforeAuto :: Bool
    ,testRequire :: [String]
    }

require :: String -> TestInfo test -> TestInfo test
require x t = t{testRequire=x:testRequire t}

threads :: Int -> TestInfo test -> TestInfo test
threads j t = t{testThreads=Just j}

threadsAll :: TestInfo test -> TestInfo test
threadsAll t = t{testThreads=Nothing}

before :: [test] -> TestInfo test -> TestInfo test
before xs t = t{testBefore=testBefore t++xs}

beforeClear :: TestInfo test -> TestInfo test
beforeClear t = t{testBefore=[], testBeforeAuto=False}

run :: IO [test] -> TestInfo test
run act = TestInfo (Just 1) act [] True []


newtype State = State String deriving Show
newtype Patch = Patch String deriving (Show,Eq)
newtype Test = Test String deriving Show

concrete :: (Show state, Read state, Show patch, Read patch, Show test, Read test)
         => Oven state patch test -> Oven State Patch Test
concrete = undefined
