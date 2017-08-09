{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Draw
  ( drawWithCurrentTool
  , drawAtPoint
  , eraseAtPoint
  , boxCorners
  , drawBox
  )
where

import Brick
import Brick.Widgets.Border.Style
import Data.Monoid ((<>))
import Lens.Micro.Platform
import Control.Monad.Trans (liftIO)
import qualified Graphics.Vty as V

import Types
import Canvas
import Util

drawWithCurrentTool :: (Int, Int) -> AppState -> EventM Name AppState
drawWithCurrentTool point s =
    case s^.tool of
        Freehand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point s
        Recolor  -> recolorAtPoint point s
        Box      -> do
            case s^.dragging of
                Nothing -> return s
                Just (n, l0, l1) ->
                    case n of
                        Canvas -> do
                            o <- liftIO $ clearCanvas (s^.drawingOverlay)
                            drawBox ascii l0 l1 drawingOverlay $
                                     s & drawingOverlay .~ o
                        _ -> return s

drawAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawMany :: [((Int, Int), Char, V.Attr)] -> Lens' AppState Canvas -> AppState -> EventM Name AppState
drawMany pixels which s = do
    let arr = s^.which
    arr' <- liftIO $ canvasSetMany arr pixels
    return $ s & which .~ arr'
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

drawBox :: BorderStyle
        -> Location
        -> Location
        -> Lens' AppState Canvas
        -> AppState
        -> EventM Name AppState
drawBox bs a b which s = do
    let attr = currentPaletteAttribute s
        (ul, lr) = boxCorners a b
        (ll, ur) = ( (ul^._1, lr^._2)
                   , (lr^._1, ul^._2)
                   )
        top =    (, bsHorizontal bs, attr) <$> (, ul^._2) <$> [ul^._1 + 1..ur^._1 - 1]
        bottom = (, bsHorizontal bs, attr) <$> (, ll^._2) <$> [ll^._1 + 1..lr^._1 - 1]
        left =   (, bsVertical bs, attr)  <$> (ul^._1, ) <$> [ul^._2 + 1..ll^._2 - 1]
        right =  (, bsVertical bs, attr)  <$> (ur^._1, ) <$> [ur^._2 + 1..lr^._2 - 1]

        -- Draw the corners
        pixels = [ (ul, bsCornerTL bs, attr)
                 , (lr, bsCornerBR bs, attr)
                 , (ll, bsCornerBL bs, attr)
                 , (ur, bsCornerTR bs, attr)
                 ] <>
                 -- Draw the top and bottom
                 top <>
                 bottom <>
                 -- Draw the sides
                 left <>
                 right

    drawMany pixels which s

boxCorners :: Location -> Location -> ((Int, Int), (Int, Int))
boxCorners (Location (a0, a1)) (Location (b0, b1)) =
    let ul = (min a0 b0, min a1 b1)
        lr = (max a0 b0, max a1 b1)
    in (ul, lr)
