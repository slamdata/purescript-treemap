module Data.Treemap.Rectangle where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)

import Data.Generic (class Generic)

-- | A rectangular region defined by: x position, y position, width, height.
data Rectangle = Rectangle Number Number Number Number

derive instance eqRectangle :: Eq Rectangle
derive instance ordRectangle :: Ord Rectangle
derive instance genericRectangle :: Generic Rectangle

instance showRectangle :: Show Rectangle where
  show (Rectangle x y w h) =
    "(Rectangle "
      <> show x <> " " <> show y <> " "
      <> show w <> " " <> show h <> ")"

-- | A rectangle associated with some data, used in the results of a treemap
-- | computation.
data Cell a = Cell Rectangle a

derive instance eqCell :: Eq a => Eq (Cell a)
derive instance ordCell :: Ord a => Ord (Cell a)
derive instance genericCell :: Generic a => Generic (Cell a)

instance showCell :: Show a => Show (Cell a) where
  show (Cell rect a) = "(Cell " <> show rect <> " " <> show a <> ")"

instance functorCell ∷ Functor Cell where
  map f (Cell rect a) = Cell rect (f a)

instance extendCell ∷ Extend Cell where
  extend f cell@(Cell rect _) = Cell rect (f cell)

instance comonadCell ∷ Comonad Cell where
  extract (Cell _ a) = a
