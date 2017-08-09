{-# LANGUAGE TupleSections #-}
module Draw
  ( drawWithCurrentTool
  , drawAtPoint
  , eraseAtPoint
  , clearCanvas
  , boxCorners
  , drawBox
  )
where

import Brick
import Data.Monoid ((<>))
import Lens.Micro.Platform
import Control.Monad.Trans (liftIO)
import qualified Graphics.Vty as V

import Types
import Canvas
import Util

clearCanvas :: AppState -> EventM Name AppState
clearCanvas s = do
    newC <- liftIO $ newCanvas $ canvasSize (s^.drawing)
    return $ s & drawing .~ newC

drawWithCurrentTool :: (Int, Int) -> AppState -> EventM Name AppState
drawWithCurrentTool point s =
    case s^.tool of
        Freehand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point s
        Recolor  -> recolorAtPoint point s
        Box      -> return s

drawAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawMany :: [((Int, Int), Char, V.Attr)] -> AppState -> EventM Name AppState
drawMany pixels s = do
    let arr = s^.drawing
    arr' <- liftIO $ canvasSetMany arr pixels
    return $ s & drawing .~ arr'
               & canvasDirty .~ True

drawAtPoint' :: (Int, Int) -> Char -> V.Attr -> AppState -> EventM Name AppState
drawAtPoint' point ch attr s = do
    let arr = s^.drawing
    arr' <- liftIO $ canvasSetPixel arr point ch attr
    return $ s & drawing .~ arr'
               & canvasDirty .~ True

eraseAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
eraseAtPoint point s =
    drawAtPoint' point ' ' V.defAttr s

recolorAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
recolorAtPoint point s = do
    let c = fst $ canvasGetPixel (s^.drawing) point
    drawAtPoint' point c (currentPaletteAttribute s) s

drawBox :: Location -> Location -> AppState -> EventM Name AppState
drawBox a b s = do
    let attr = currentPaletteAttribute s
        corner = '+'
        vert = '|'
        horiz = '-'
        (ul, lr) = boxCorners a b
        (ll, ur) = ( (ul^._1, lr^._2)
                   , (lr^._1, ul^._2)
                   )
        top =    (, horiz, attr) <$> (, ul^._2) <$> [ul^._1 + 1..ur^._1 - 1]
        bottom = (, horiz, attr) <$> (, ll^._2) <$> [ll^._1 + 1..lr^._1 - 1]
        left =   (, vert, attr)  <$> (ul^._1, ) <$> [ul^._2 + 1..ll^._2 - 1]
        right =  (, vert, attr)  <$> (ur^._1, ) <$> [ur^._2 + 1..lr^._2 - 1]

        -- Draw the corners
        pixels = [ (ul, corner, attr)
                 , (lr, corner, attr)
                 , (ll, corner, attr)
                 , (ur, corner, attr)
                 ] <>
                 -- Draw the top and bottom
                 top <>
                 bottom <>
                 -- Draw the sides
                 left <>
                 right

    drawMany pixels s

boxCorners :: Location -> Location -> ((Int, Int), (Int, Int))
boxCorners (Location (a0, a1)) (Location (b0, b1)) =
    let ul = (min a0 b0, min a1 b1)
        lr = (max a0 b0, max a1 b1)
    in (ul, lr)
