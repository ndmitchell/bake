module Bake.Test.GCSpec where

import Test.Tasty
import Test.Tasty.Hspec
import System.Directory.Extra
import System.FilePath
import System.Directory
import System.IO
import System.IO.Extra
import Control.Exception
import Control.Monad
import qualified Data.Set as Set

import Development.Bake

withTestDirectory :: (FilePath -> IO ()) -> IO ()
withTestDirectory action = withTempDir $ \ dir -> bracket
                                             (do
                                                 let sourceTestDir = "test" </> "test-gc"
                                                 allFiles <- listFilesRecursive sourceTestDir
                                                 forM_ allFiles (\ file -> let file' = dir </> makeRelative sourceTestDir file
                                                                           in createDirectoryIfMissing True (takeDirectory file') >> copyFile file file')
                                                 return dir
                                             )
                                             removeDirectoryRecursive
                                             action
  
gcSpec = describe "Garbage Collector" $ do

  it "preserve incremental build test directories" $ withTestDirectory $ \ dir -> do
    garbageCollect 0 1 0 [ dir ]
    filesAfterGc <- listFilesRecursive dir

    Set.fromList (map (makeRelative dir) filesAfterGc) `shouldBe` Set.fromList ["bake-incremental.txt"
                                                                               ,"bake-extra-467723236138728164/.bake.name"
                                                                               ,"bake-store/.bake.name"
                                                                               ,"bake-test-467723236138728164/.bake.name"
                                                                               ,"bake-update-467723236138728164/.bake.name"]
    
              

  
