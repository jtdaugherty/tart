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
  , undo
  , redo
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

undo :: AppState -> EventM Name AppState
undo s =
    case s^.undoStack of
        [] -> return s
        (next:rest) -> do
            let next' = (\(p, (ch, attr)) -> (p, ch, attr)) <$> next
            (s', old) <- drawMany next' drawing s
            -- Avoid using pushUndo here, since it blows away the redo
            -- stack.
            return $ s' & undoStack .~ rest
                        & redoStack %~ (old:)

redo :: AppState -> EventM Name AppState
redo s =
    case s^.redoStack of
        [] -> return s
        (next:rest) -> do
            let next' = (\(p, (ch, attr)) -> (p, ch, attr)) <$> next
            (s', old) <- drawMany next' drawing s
            return $ s' & redoStack .~ rest
                        & undoStack %~ (old:)

drawWithCurrentTool :: (Int, Int) -> AppState -> EventM Name AppState
drawWithCurrentTool point s =
    case s^.tool of
        Freehand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point (s^.eraserSize) s
        Repaint  -> repaintAtPoint point (s^.repaintSize) s
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
                            (s', old) <- drawBox bs l0 l1 drawingOverlay $
                                     s & drawingOverlay .~ o
                            return $ pushUndo old s'
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
    (s', old) <- drawMany many drawing s
    return $ pushUndo old s'

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
floodFillAtPoint point s = do
    let fillAttr = currentPaletteAttribute s
        fillCh = s^.drawCharacter
        fillPix = (fillCh, fillAttr)
        targetPix = canvasGetPixel (s^.drawing) point
        (w, h) = canvasSize (s^.drawing)
        up    = (& _2 %~ (max 0 . pred))
        down  = (& _2 %~ (min (h-1) . succ))
        left  = (& _1 %~ (max 0 . pred))
        right = (& _1 %~ (min (w-1) . succ))

        go :: (Int, Int) -> (AppState, [((Int, Int), (Char, V.Attr))]) -> EventM Name (AppState, [((Int, Int), (Char, V.Attr))])
        go p (st, uBuf) = do
            let pix = canvasGetPixel (st^.drawing) p
            if | pix == fillPix -> return (st, uBuf)
               | pix /= targetPix -> return (st, uBuf)
               | otherwise -> do
                   let old = canvasGetPixel (st^.drawing) p
                   d' <- liftIO $ canvasSetPixel (st^.drawing) p fillCh fillAttr
                   let newSt = st & drawing .~ d' & canvasDirty .~ True
                   go (down p) (newSt, (p, old):uBuf) >>=
                       go (up p) >>=
                       go (left p) >>=
                       go (right p)

    (finalSt, undoBuf) <- go point (s, [])
    return $ pushUndo undoBuf finalSt

drawAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawAtPoint' :: (Int, Int) -> Char -> V.Attr -> AppState -> EventM Name AppState
drawAtPoint' point ch attr s = do
    (s', old) <- drawMany [(point, ch, attr)] drawing s
    return $ pushUndo old s'

drawMany :: [((Int, Int), Char, V.Attr)]
         -> Lens' AppState Canvas
         -> AppState
         -> EventM Name (AppState, [((Int, Int), (Char, V.Attr))])
drawMany pixels which s = do
    let arr = s^.which
        old = getOld <$> pixels
        getOld (oldLoc, _, _) = (oldLoc, canvasGetPixel (s^.which) oldLoc)
    arr' <- liftIO $ canvasSetMany arr pixels
    let newSt = s & which .~ arr'
                  & canvasDirty %~ (|| (not $ null pixels))
    return (newSt, old)

makeBoxAboutPoint :: (Int, Int) -> Int -> [(Int, Int)]
makeBoxAboutPoint point sz =
    if sz <= 0
       then []
       else let len = (sz * 2) - 1
                off = negate $ sz - 1
                noOffset = [(c, r) | r <- [0..len-1], c <- [0..len-1]]
                addOffset (c, r) = (c + off + point^._1
                                   ,r + off + point^._2
                                   )
            in addOffset <$> noOffset

eraseAtPoint :: (Int, Int) -> Int -> AppState -> EventM Name AppState
eraseAtPoint point sz s = do
    let points = makeBoxAboutPoint point sz
        pixels = (, ' ', V.defAttr) <$> points
    (s', old) <- drawMany pixels drawing s
    return $ pushUndo old s'

repaintAtPoint :: (Int, Int) -> Int -> AppState -> EventM Name AppState
repaintAtPoint point sz s = do
    let points = makeBoxAboutPoint point sz
        attr = currentPaletteAttribute s
        getPixel p = let old = canvasGetPixel (s^.drawing) p
                     in (p, old^._1, attr)
        pixels = getPixel <$> points
    (s', old) <- drawMany pixels drawing s
    return $ pushUndo old s'

drawBox :: BorderStyle
        -> Location
        -> Location
        -> Lens' AppState Canvas
        -> AppState
        -> EventM Name (AppState, [((Int, Int), (Char, V.Attr))])
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
