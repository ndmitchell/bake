{-# LANGUAGE RecordWildCards, Rank2Types, TupleSections, NoMonomorphismRestriction #-}

-- | Simple lenses
module General.Lens(
    Lens, makeLens, view, set, over, (&),
    (^.), (.~), (%~),
    at, at_, atm, _1, _2
    ) where

import Control.Applicative
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Prelude


infixl 8 ^.
infixl 1 &
infixr 4 .~, %~


type Lens s a = forall f . Functor f => (a -> f a) -> s -> f s


makeLens :: (s -> a) -> (a -> s -> s) -> Lens s a
makeLens sel upd f s = flip upd s <$> f (sel s)

view :: Lens s a -> s -> a
view l s = getConst $ l Const s

set :: Lens s a -> a -> s -> s
set l x = over l (const x)

over :: Lens s a -> (a -> a) -> s -> s
over l f s = runIdentity $ l (Identity . f) s

(^.) :: s -> Lens s a -> a
s ^. l = getConst $ l Const s
-- flip view doesn't work in 7.10, I guess because the forall gets lifted to the top

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(.~) :: Lens s a -> a -> s -> s
(.~) = set

(%~) :: Lens s a -> (a -> a) -> s -> s
(%~) = over


at :: Ord k => k -> Lens (Map.Map k v) (Maybe v)
at k f mp = maybe (Map.delete k mp) (\v -> Map.insert k v mp) <$> f (Map.lookup k mp)

at_ :: Ord k => k -> v -> Lens (Map.Map k v) v
at_ k d = at k . makeLens (fromMaybe d) (\x _ -> Just x)

atm :: (Ord k, Monoid v) => k -> Lens (Map.Map k v) v
atm k = at_ k mempty

_1 :: Lens (a, b) a
_1 f (a, b) = (,b) <$> f a

_2 :: Lens (a, b) b
_2 f (a, b) = (a,) <$> f b
