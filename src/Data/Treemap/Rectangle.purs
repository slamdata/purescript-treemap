module Data.Treemap.Rectangle where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)

-- | A rectangular region defined by: x position, y position, width, height.
data Rectangle = Rectangle Number Number Number Number

-- | A rectangle associated with some data, used in the results of a treemap
-- | computation.
data Cell a = Cell Rectangle a

instance functorCell ∷ Functor Cell where
  map f (Cell rect a) = Cell rect (f a)

instance extendCell ∷ Extend Cell where
  extend f cell@(Cell rect _) = Cell rect (f cell)

instance comonadCell ∷ Comonad Cell where
  extract (Cell _ a) = a
