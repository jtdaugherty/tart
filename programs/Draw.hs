{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Draw
  ( drawWithCurrentTool
  , drawAtPoint
  , eraseAtPoint
  , boxCorners
  , drawBox
  , drawTextAtPoint
  , truncateText
  , pasteTextAtPoint
  )
where

import Brick
import Brick.Widgets.Border.Style
import Data.Monoid ((<>))
import Lens.Micro.Platform
import Control.Monad.Trans (liftIO)
import Control.Monad (foldM)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

import Types
import Tart.Canvas
import Util

drawWithCurrentTool :: (Int, Int) -> AppState -> EventM Name AppState
drawWithCurrentTool point s =
    case s^.tool of
        Freehand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point s
        Recolor  -> recolorAtPoint point s
        FloodFill -> floodFillAtPoint point s
        TextString -> return $ beginTextEntry point s
        Box -> do
            case s^.dragging of
                Nothing -> return s
                Just (n, l0, l1) ->
                    case n of
                        Canvas -> do
                            let bs = snd $ getBoxBorderStyle s
                            o <- liftIO $ clearCanvas (s^.drawingOverlay)
                            drawBox bs l0 l1 drawingOverlay $
                                     s & drawingOverlay .~ o
                        _ -> return s
        Eyedropper ->
            -- Read the pixel at the canvas location. Set the
            -- application state's current drawing character and colors
            -- from it.
            let (ch, attr) = canvasGetPixel (s^.drawing) point
            in return $ s & drawCharacter .~ ch
                          & drawFgPaletteIndex .~ findFgPaletteEntry attr s
                          & drawBgPaletteIndex .~ findBgPaletteEntry attr s

truncateText :: (Int, Int) -> T.Text -> AppState -> T.Text
truncateText point t s =
    let startCol = point^._1
        maxCol = min ((canvasSize (s^.drawing))^._1 - 1)
                     (startCol + T.length t - 1)
        safe = T.take (maxCol - startCol + 1) t
    in safe

pasteTextAtPoint :: (Int, Int) -> AppState -> T.Text -> EventM Name AppState
pasteTextAtPoint point s t = do
    let ls = T.lines t
        (startCol, startRow) = point
        pasteWidth = maximum $ T.length <$> ls
        pasteHeight = length ls
        (oldWidth, oldHeight) = canvasSize (s^.drawing)
        newSize = ( max oldWidth pasteWidth
                  , max oldHeight pasteHeight
                  )
        pairs = zip [startRow..] ls

    s' <- resizeCanvas s newSize
    foldM (\st (row, line) -> drawTextAtPoint (startCol, row) line st) s' pairs

drawTextAtPoint :: (Int, Int) -> T.Text -> AppState -> EventM Name AppState
drawTextAtPoint point t s = do
    let attr = currentPaletteAttribute s
        (startCol, row) = point
        pixs = zip ([startCol..]) (T.unpack $ truncateText point t s)
        many = mkEntry <$> pixs
        mkEntry (col, ch) = ((col, row), ch, attr)
    drawMany many drawing s

findFgPaletteEntry :: V.Attr -> AppState -> Int
findFgPaletteEntry a s =
    let fgc = case V.attrForeColor a of
          V.KeepCurrent -> Nothing
          V.Default -> Nothing
          V.SetTo c -> Just c
    in maybe 0 id $ Vec.findIndex (== fgc) (s^.palette)

findBgPaletteEntry :: V.Attr -> AppState -> Int
findBgPaletteEntry a s =
    let bgc = case V.attrBackColor a of
          V.KeepCurrent -> Nothing
          V.Default -> Nothing
          V.SetTo c -> Just c
    in maybe 0 id $ Vec.findIndex (== bgc) (s^.palette)

floodFillAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
floodFillAtPoint point s =
    let fillAttr = currentPaletteAttribute s
        fillCh = s^.drawCharacter
        fillPix = (fillCh, fillAttr)
        targetPix = canvasGetPixel (s^.drawing) point
        (w, h) = canvasSize (s^.drawing)
        up    = (& _2 %~ (max 0 . pred))
        down  = (& _2 %~ (min (h-1) . succ))
        left  = (& _1 %~ (max 0 . pred))
        right = (& _1 %~ (min (w-1) . succ))
        go p st = do
            let pix = canvasGetPixel (st^.drawing) p
            if | pix == fillPix -> return st
               | pix /= targetPix -> return st
               | otherwise -> do
                   d' <- liftIO $ canvasSetPixel (st^.drawing) p fillCh fillAttr
                   go (down p) (st & drawing .~ d' & canvasDirty .~ True) >>=
                       go (up p) >>=
                       go (left p) >>=
                       go (right p)

    in go point s

drawAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawMany :: [((Int, Int), Char, V.Attr)] -> Lens' AppState Canvas -> AppState -> EventM Name AppState
drawMany pixels which s = do
    let arr = s^.which
    arr' <- liftIO $ canvasSetMany arr pixels
    return $ s & which .~ arr'
               & canvasDirty %~ (|| (not $ null pixels))

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

        width = lr^._1 - ul^._1
        height = lr^._2 - ul^._2
        corners = if width == 0
                  then [ (ul, bsVertical bs, attr)
                       , (lr, bsVertical bs, attr)
                       ]
                  else if height == 0
                       then [ (ul, bsHorizontal bs, attr)
                            , (lr, bsHorizontal bs, attr)
                            ]
                       else [ (ul, bsCornerTL bs, attr)
                            , (lr, bsCornerBR bs, attr)
                            , (ll, bsCornerBL bs, attr)
                            , (ur, bsCornerTR bs, attr)
                            ]

        -- Draw the corners
        pixels = corners <>
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
