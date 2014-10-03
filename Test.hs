
module Test(main) where

import Development.Bake
import Development.Bake.Git
import Development.Shake.Command
import System.Environment


---------------------------------------------------------------------
-- NORMAL CODE

data Platform = Linux | Windows deriving (Show,Read)
data Action = Compile | Run Int deriving (Show,Read)


main :: IO ()
main = do
    args <- getArgs
    if null args then test else bake $
        ovenGit "https://github.com/ndmitchell/bake.git" "master" $
        ovenTest readShowStringy (return allTests) execute
        defaultOven

allTests = [(p,t) | p <- [Linux,Windows], t <- Compile : map Run [1..3]]

execute :: (Platform,Action) -> TestInfo (Platform,Action)
execute (p,Compile) = matchOS p $ run $ do
    cmd "ghc --make Main.hs"
execute (p,Run i) = require [(p,Compile)] $ matchOS p $ run $ do
    cmd "Main" (show i)

matchOS :: Platform -> TestInfo t -> TestInfo t
matchOS p = suitable (fmap (== show p) $ getEnv "PLATFORM")


---------------------------------------------------------------------
-- TEST BITS

test :: IO ()
test = return ()
