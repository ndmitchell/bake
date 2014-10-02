
module Test(main) where

import Development.Bake
import Development.Bake.Git
import Development.Shake.Command
import System.Environment


---------------------------------------------------------------------
-- NORMAL CODE

data Platform = Linux | Windows deriving (Show,Read)
data Action = Compile | Run deriving (Show,Read)


main :: IO ()
main = do
    args <- getArgs
    if null args then test else bake $
        ovenGit "https://github.com/ndmitchell/bake.git" "master" $
        ovenTest readShowStringy (const execute)
        defaultOven


execute :: Maybe (Platform,Action) -> TestInfo (Platform,Action)
execute Nothing = run $ do
    return [(Linux,Compile),(Windows,Compile)]
execute (Just (p,Compile)) = matchOS p $ run $ do
    () <- cmd "ghc --make Main.hs"
    return [(p,Run)]
execute (Just (p,Run)) = matchOS p $ run $ do
    () <- cmd "Main"
    return []

matchOS :: Platform -> TestInfo t -> TestInfo t
matchOS p = suitable (fmap (== show p) $ getEnv "PLATFORM")


---------------------------------------------------------------------
-- TEST BITS

test :: IO ()
test = return ()
