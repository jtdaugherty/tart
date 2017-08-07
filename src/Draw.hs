module Draw
  ( drawWithCurrentTool
  , drawAtPoint
  , eraseAtPoint
  , clearCanvas
  )
where

import Brick
import Lens.Micro.Platform
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Types

clearCanvas :: AppState -> EventM Name AppState
clearCanvas s =
    return $ s & drawing .~ (Vec.replicate (s^.canvasSize._2) $
                             Vec.replicate (s^.canvasSize._1) blankPixel)

drawWithCurrentTool :: (Int, Int) -> AppState -> EventM Name AppState
drawWithCurrentTool point s =
    case s^.tool of
        FreeHand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point s

drawAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawAtPoint' :: (Int, Int) -> Char -> V.Attr -> AppState -> EventM Name AppState
drawAtPoint' point ch attr s =
    return $ s & drawing.ix (point^._2).ix (point^._1) .~ encodePixel ch attr

eraseAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
eraseAtPoint point s =
    drawAtPoint' point ' ' V.defAttr s

currentPaletteAttribute :: AppState -> V.Attr
currentPaletteAttribute s =
    let PaletteEntry mkFg _ = Vec.unsafeIndex (s^.palette) (s^.drawFgPaletteIndex)
        PaletteEntry _ mkBg = Vec.unsafeIndex (s^.palette) (s^.drawBgPaletteIndex)
    in mkFg $ mkBg V.defAttr
