{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Core.Send(
    sendAddPatch, sendDelPatch, sendSetState,
    sendPause, sendUnpause, sendRequeue,
    sendAddSkip, sendDelSkip
    ) where

import Control.Monad
import Development.Bake.Core.Type
import Development.Bake.Core.Message

sendPause :: (Host,Port) -> Author -> IO ()
sendPause hp author = void $ sendMessage hp $ Pause author

sendUnpause :: (Host,Port) -> Author -> IO ()
sendUnpause hp author = void $ sendMessage hp $ Unpause author

sendAddPatch :: (Host,Port) -> Author -> String -> IO ()
sendAddPatch hp author x = void $ sendMessage hp $ AddPatch author $ toPatch x

sendDelPatch :: (Host,Port) -> Author -> String -> IO ()
sendDelPatch hp author x = void $ sendMessage hp $ DelPatch author $ toPatch x

sendRequeue :: (Host,Port) -> Author -> IO ()
sendRequeue hp author = void $ sendMessage hp $ Requeue author

sendAddSkip :: (Host,Port) -> Author -> String -> IO ()
sendAddSkip hp author x = void $ sendMessage hp $ AddSkip author $ toTest x

sendDelSkip :: (Host,Port) -> Author -> String -> IO ()
sendDelSkip hp author x = void $ sendMessage hp $ DelSkip author $ toTest x

sendSetState :: (Host,Port) -> Author -> String -> IO ()
sendSetState hp author x = void $ sendMessage hp $ SetState author $ toState x
