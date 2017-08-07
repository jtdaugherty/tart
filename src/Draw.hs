module Draw
  ( drawWithCurrentTool
  , drawAtPoint
  , eraseAtPoint
  , clearCanvas
  )
where

import Lens.Micro.Platform
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Types

clearCanvas :: AppState -> AppState
clearCanvas s =
    s & drawing .~ (Vec.replicate (s^.canvasSize._2) $
                    Vec.replicate (s^.canvasSize._1) blankPixel)

drawWithCurrentTool :: (Int, Int) -> AppState -> AppState
drawWithCurrentTool point s =
    case s^.tool of
        FreeHand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point s

drawAtPoint :: (Int, Int) -> AppState -> AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawAtPoint' :: (Int, Int) -> Char -> V.Attr -> AppState -> AppState
drawAtPoint' point ch attr s =
    s & drawing.ix (point^._2).ix (point^._1) .~ (ch, attr)

eraseAtPoint :: (Int, Int) -> AppState -> AppState
eraseAtPoint point s = drawAtPoint' point ' ' V.defAttr s

currentPaletteAttribute :: AppState -> V.Attr
currentPaletteAttribute s =
    let fg = Vec.unsafeIndex (s^.palette) (s^.drawFgPaletteIndex)
        bg = Vec.unsafeIndex (s^.palette) (s^.drawBgPaletteIndex)
    in V.defAttr `V.withForeColor` fg
                 `V.withBackColor` bg
