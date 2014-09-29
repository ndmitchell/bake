
module Example(main) where

import Development.Bake
import Development.Bake.Git
import Development.Bake.Email
import Development.Shake.Command


data Step
    = Test String
    | Benchmark
      deriving (Show,Read)


main :: IO ()
main = bake $
    ovenEmail ("smtp.server.com",25) $
    ovenGit "repo@git.com" "master" $
    ovenTest readShowStringy (const execute)
    defaultOven


execute :: Maybe Step -> TestInfo Step
execute Nothing = threadsAll $ run $ do
    () <- cmd "ghc --make -j"
    tests <- readFile "tests.txt"
    return $ Benchmark : map Test (lines tests)
execute (Just Benchmark) = run $ do
    () <- cmd "runhaskell Benchmark"
    return []
execute (Just (Test test)) = run $ do
    () <- cmd "runhaskell Main" test
    return []
