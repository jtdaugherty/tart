module Draw
  ( drawWithCurrentTool
  , drawAtPoint
  , eraseAtPoint
  , clearCanvas
  )
where

import Brick
import Lens.Micro.Platform
import Control.Monad.Trans (liftIO)
import qualified Data.Array.MArray as A
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Types

clearCanvas :: AppState -> EventM Name AppState
clearCanvas s = do
    let newBounds = ((0, 0), ((s^.canvasSize) & each %~ pred))
    liftIO $ do
        newDraw <- A.newArray newBounds blankPixel
        newFreeze <- A.freeze newDraw
        return $ s & drawing .~ newDraw
                   & drawingFrozen .~ newFreeze

drawWithCurrentTool :: (Int, Int) -> AppState -> EventM Name AppState
drawWithCurrentTool point s =
    case s^.tool of
        FreeHand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point s

drawAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawAtPoint' :: (Int, Int) -> Char -> V.Attr -> AppState -> EventM Name AppState
drawAtPoint' point ch attr s = refreeze $ do
    let arr = s^.drawing
    liftIO $ A.writeArray arr point $ encodePixel ch attr
    return s

eraseAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
eraseAtPoint point s =
    drawAtPoint' point ' ' V.defAttr s

currentPaletteAttribute :: AppState -> V.Attr
currentPaletteAttribute s =
    let PaletteEntry mkFg _ = Vec.unsafeIndex (s^.palette) (s^.drawFgPaletteIndex)
        PaletteEntry _ mkBg = Vec.unsafeIndex (s^.palette) (s^.drawBgPaletteIndex)
    in mkFg $ mkBg V.defAttr

refreeze :: EventM Name AppState -> EventM Name AppState
refreeze mkS = do
    s <- mkS
    liftIO $ do
        f <- A.freeze $ s^.drawing
        return $ s & drawingFrozen .~ f
