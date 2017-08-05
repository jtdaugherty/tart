module UI.Main
  ( drawMainUI
  )
where

import Brick
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import qualified Data.Array.Unboxed as A
import Lens.Micro.Platform

import Types

drawMainUI :: AppState -> [Widget Name]
drawMainUI s =
    [ hud s
    , canvas s
    ]

hud :: AppState -> Widget Name
hud s =
    str $ "[tool:" <> show (s^.tool) <> "]"

canvas :: AppState -> Widget Name
canvas s = clickable Canvas $ raw $ canvasToImage $ s^.drawingFrozen

canvasToImage :: A.UArray Coord Char -> V.Image
canvasToImage a =
    let (_, (lastCol, lastRow)) = A.bounds a
        rows = getRow <$> [0..lastRow]
        getRow r = V.string V.defAttr (getCol r <$> [0..lastCol])
        getCol r c = a A.! (c, r)
    in V.vertCat rows
