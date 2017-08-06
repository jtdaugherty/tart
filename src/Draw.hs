module Draw
  ( drawAtPoint
  )
where

import Lens.Micro.Platform
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Types

drawAtPoint :: (Int, Int) -> AppState -> AppState
drawAtPoint point s =
    let ch = s^.drawCharacter
        attr = V.defAttr `V.withForeColor` (Vec.unsafeIndex (s^.palette) (s^.drawPaletteIndex))
    in s & drawing.ix (point^._2).ix (point^._1) .~ (ch, attr)
