
module Development.Bake.Send(
    sendPause, sendUnpause,
    sendAddPatch, sendDelPatch, sendDelAllPatches
    ) where

import Control.Monad
import Development.Bake.Type
import Development.Bake.Message

sendPause :: (Host,Port) -> Author -> IO ()
sendPause hp author = void $ sendMessage hp $ Pause author

sendUnpause :: (Host,Port) -> Author -> IO ()
sendUnpause hp author = void $ sendMessage hp $ Unpause author

sendAddPatch :: Show patch => (Host,Port) -> Author -> patch -> IO ()
sendAddPatch hp author x = void $ sendMessage hp $ AddPatch author $ Patch $ show x

sendDelPatch :: Show patch => (Host,Port) -> Author -> patch -> IO ()
sendDelPatch hp author x = void $ sendMessage hp $ DelPatch author $ Patch $ show x

sendDelAllPatches :: (Host,Port) -> Author -> IO ()
sendDelAllPatches hp author = void $ sendMessage hp $ DelAllPatches author
