module UI.Main
  ( drawMainUI
  )
where

import Brick
import qualified Graphics.Vty as V
import qualified Data.Array.Unboxed as A
import Lens.Micro.Platform

import Types

drawMainUI :: AppState -> [Widget Name]
drawMainUI s =
    [clickable Canvas $ raw $ canvasToImage $ s^.drawingFrozen]

canvasToImage :: A.UArray Coord Char -> V.Image
canvasToImage a =
    let (_, (lastCol, lastRow)) = A.bounds a
        rows = getRow <$> [0..lastRow]
        getRow r = V.string V.defAttr (getCol r <$> [0..lastCol])
        getCol r c = a A.! (c, r)
    in V.vertCat rows
