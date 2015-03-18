{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Core.Send(
    sendPause, sendUnpause,
    sendAddPatch, sendDelPatch, sendDelAllPatches, sendRequeue
    ) where

import Control.Monad
import Development.Bake.Core.Type
import Development.Bake.Core.Message

sendPause :: (Host,Port) -> Author -> IO ()
sendPause hp author = void $ sendMessage hp $ Pause author

sendUnpause :: (Host,Port) -> Author -> IO ()
sendUnpause hp author = void $ sendMessage hp $ Unpause author

sendAddPatch :: (Host,Port) -> Author -> String -> IO ()
sendAddPatch hp author x = void $ sendMessage hp $ AddPatch author $ Patch x

sendDelPatch :: (Host,Port) -> Author -> String -> IO ()
sendDelPatch hp author x = void $ sendMessage hp $ DelPatch author $ Patch x

sendDelAllPatches :: (Host,Port) -> Author -> IO ()
sendDelAllPatches hp author = void $ sendMessage hp $ DelAllPatches author

sendRequeue :: (Host,Port) -> Author -> IO ()
sendRequeue hp author = void $ sendMessage hp $ Requeue author
