module Draw
  ( drawAtPoint
  )
where

import Brick
import Lens.Micro.Platform
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Types

drawAtPoint :: AppState -> (Int, Int) -> EventM Name AppState
drawAtPoint s point =
    return $ s & drawing.ix (point^._2).ix (point^._1) .~
                   ('x', V.defAttr `V.withForeColor` (Vec.unsafeIndex (s^.palette) (s^.drawPaletteIndex)))
