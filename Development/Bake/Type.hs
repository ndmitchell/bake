{-# LANGUAGE Rank2Types #-}

-- | Define a continuous integration system.
module Development.Bake.Type(
    Host, Port,
    Candidate(..), Oven(..), TestInfo(..), defaultOven,
    threads, threadsAll, before, beforeClear, run,
    State(..), Patch(..), Test(..), concrete,
    Author
    ) where

type Author = String

type Host = String

type Port = Int

data Candidate state patch = Candidate state [patch]

data Oven state patch test = Oven
    {ovenUpdateState :: Maybe (Candidate state patch) -> IO state
        -- ^ Given a state, and a set of candiates that have passed,
        --   merge to create a new state.
    ,ovenRunTest :: Candidate state patch -> Maybe test -> TestInfo test
        -- ^ Produce information about a test
    ,ovenPatchReject :: patch -> Maybe test -> IO ()
        -- ^ A patch has been marked as failing, tell everyone.
    ,ovenPatchExtra :: patch -> IO String
        -- ^ Extra information about the patch
    ,ovenDefaultServer :: (Host, Port)
        -- ^ Default server to use
    }

defaultOven :: Oven state patch test
defaultOven = Oven
    (error "defaultOven.ovenUpdateState") (error "defaultOven.ovenRunTest")
    (\_ _ -> return ()) (\_ -> return "") ("",0)

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


newtype State = State String
newtype Patch = Patch String
newtype Test = Test String

concrete :: (Show state, Read state, Show patch, Read patch, Show test, Read test)
         => Oven state patch test -> Oven State Patch Test
concrete = undefined
