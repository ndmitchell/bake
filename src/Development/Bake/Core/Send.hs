{-# LANGUAGE RecordWildCards #-}

module Development.Bake.Core.Send(
    sendAddPatch, sendDelPatch, sendSetState,
    sendPause, sendUnpause, sendRequeue,
    sendAddSkip, sendDelSkip
    ) where

import Control.Monad
import Development.Bake.Core.Type
import Development.Bake.Core.Message

sendPause :: (Host,Port) -> IO ()
sendPause hp = void $ sendMessage hp Pause

sendUnpause :: (Host,Port) -> IO ()
sendUnpause hp = void $ sendMessage hp Unpause

sendAddPatch :: (Host,Port) -> Author -> String -> IO ()
sendAddPatch hp author x = void $ sendMessage hp $ AddPatch author $ toPatch x

sendDelPatch :: (Host,Port) -> String -> IO ()
sendDelPatch hp x = void $ sendMessage hp $ DelPatch $ toPatch x

sendRequeue :: (Host,Port) -> IO ()
sendRequeue hp = void $ sendMessage hp Requeue

sendAddSkip :: (Host,Port) -> Author -> String -> IO ()
sendAddSkip hp author x = void $ sendMessage hp $ AddSkip author $ toTest x

sendDelSkip :: (Host,Port) -> String -> IO ()
sendDelSkip hp x = void $ sendMessage hp $ DelSkip $ toTest x

sendSetState :: (Host,Port) -> Author -> String -> IO ()
sendSetState hp author x = void $ sendMessage hp $ SetState author $ toState x
