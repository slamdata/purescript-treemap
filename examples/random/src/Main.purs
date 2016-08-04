module Example.Random where

import Prelude

import Control.Monad.Eff.Random (RANDOM, randomRange)
import Control.Monad.Eff.Class (liftEff)

import Data.Array as A
import Data.List as L
import Data.Const (Const, getConst)
import Data.Treemap.Squarified (Cell(..), Rectangle(..), treemap)
import Data.Unfoldable (replicateA)

import Control.Monad.Eff (Eff)

import CSS as CSS

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.CSS.Indexed as HCSS
import Halogen.Util (awaitBody, runHalogenAff)

type Query = Const Void

type State = L.List Number

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    let
      rects = treemap id state (Rectangle 0.0 0.0 640.0 480.0)
    in
      HH.div
        [ HP.classes [ HH.className "container"] ]
        $ map renderCell $ A.fromFoldable rects

  renderCell :: forall a. Cell a -> H.ComponentHTML Query
  renderCell (Cell (Rectangle x y width height) _) =
    HH.div
      [ HCSS.style style
      , HP.classes [ HH.className "rect" ]
      ] []
    where
    style = do
      CSS.position CSS.absolute
      CSS.left (CSS.px x)
      CSS.top (CSS.px y)
      CSS.width (CSS.px width)
      CSS.height (CSS.px height)

  eval :: Query ~> H.ComponentDSL State Query g
  eval = absurd <<< getConst

main :: Eff (H.HalogenEffects (random :: RANDOM)) Unit
main = runHalogenAff do
  body <- awaitBody
  weights <- liftEff $ replicateA 20 (randomRange 0.2 20.0)
  H.runUI ui (weights) body
