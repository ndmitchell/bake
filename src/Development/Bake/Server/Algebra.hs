{-# LANGUAGE RecordWildCards #-}

-- | Define a continuous integration system.
module Development.Bake.Server.Algebra(
    targetBlessedPrefix, blessedState,
    ) where

import Development.Bake.Core.Type
import Development.Bake.Core.Message
import Development.Bake.Server.Type
import Development.Bake.Server.Query
import Control.Applicative
import Data.List.Extra


-- | Given the current target, what prefix is already blessed.
--   Usually the empty list, can immediately be rolled into the target.
targetBlessedPrefix :: Server -> [Patch]
targetBlessedPrefix server@Server{..} = head $ filter isBlessed $ reverse $ inits $ snd target
    where
        isBlessed [] = True
        isBlessed ps = blessedState server (fst target, ps)


-- | Given a State, is it blessed
blessedState :: Server -> (State, [Patch]) -> Bool
blessedState server c
    | let f t = answered server [test' t, success', candidate' c]
    , todo:_ <- aTests . snd <$> f Nothing
    = all (not . null . f . Just) todo
blessedState _ _ = False
