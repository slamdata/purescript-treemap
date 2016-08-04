-- | A treemap algorithm based on the paper ["Squarified Treemaps" by
-- | Mark Bruls, Kees Huizing, and Jarke J. van Wijk][1].
-- |
-- | [1]: http://www.win.tue.nl/~vanwijk/stm.pdf
module Data.Treemap.Squarified
  ( treemap
  , module Data.Treemap.Rectangle
  ) where

import Prelude

import Data.Foldable as F
import Data.Function (on)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (maybe)
import Data.Treemap.Rectangle (Rectangle(..), Cell(..))
import Data.Tuple (Tuple(..), snd)

-- | Computes a treemap for a dataset. For a recursive treemap this will need to
-- | be run for each subtree as it only considers one dimension at a time.
-- |
-- | - The `a → Number` function is used to determine the weight of a node in
-- |   the treemap. The behaviour of the algorithm is unspecified if any weight
-- |   is not a positive number.
-- | - The `List a` is the dataset to compute from.
-- | - The `Rectangle` is the bounding area for the treemap.
treemap ∷ ∀ a. (a → Number) → List a → Rectangle → List (Cell a)
treemap f xs rect@(Rectangle _ _ width height) =
  let scale = width * height / F.sum (f <$> xs)
  in squarify f scale xs rect

squarify ∷ ∀ a. (a → Number) → Number → List a → Rectangle → List (Cell a)
squarify f scale xs rect =
  let sorted = L.sortBy (flip compare `on` f) xs
  in join $ go Nil rect sorted Nil 0.0 (shortestSide rect)
  where

  go
    ∷ List (List (Cell a))
    → Rectangle
    → List a
    → List a
    → Number
    → Number
    → List (List (Cell a))
  go result rect xs row s w =
    case xs of
      x : xs' →
        let
          row' = x : row
          s' = s + f x
        in
          if L.null row || worst row s w >= worst row' s' w
          then go result rect xs' row' s' w
          else
            let
              rect' = trim (s * scale) rect
              result' = layout row (s * scale) rect : result
            in
              go result' rect' xs Nil 0.0 (shortestSide rect')
      _ → layout row (s * scale) rect : result

  worst ∷ List a → Number → Number → Number
  worst row s w =
    let
      s2 = s * s
      w2 = w * w / scale
      rMin = maybe 0.0 f (L.head row)
      rMax = maybe 0.0 f (L.last row)
    in
      max (w2 * rMax / s2) (s2 / (w2 * rMin))

  layout ∷ List a → Number → Rectangle → List (Cell a)
  layout row s (Rectangle x y width height) =
    if width >= height
    then
      let aw = s / height
      in
        snd $ F.foldl
          (\(Tuple offset result) r →
            let h = f r * scale / aw
            in Tuple (offset + h) (Cell (Rectangle x offset aw h) r : result))
          (Tuple y Nil)
          row
    else
      let ah = s / width
      in
        snd $ F.foldl
          (\(Tuple offset result) r →
            let w = f r * scale / ah
            in Tuple (offset + w) (Cell (Rectangle offset y w ah) r : result))
          (Tuple x Nil)
          row

shortestSide ∷ Rectangle → Number
shortestSide (Rectangle _ _ width height) = min width height

trim ∷ Number → Rectangle → Rectangle
trim s (Rectangle x y width height) =
  if width >= height
  then
    let aw = s / height
    in Rectangle (x + aw) y (width - aw) height
  else
    let ah = s / width
    in Rectangle x (y + ah) width (height - ah)
