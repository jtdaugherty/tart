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
import Data.Maybe (catMaybes)

import Types
import Tart.Canvas
import State

undo :: AppState -> EventM Name AppState
undo s =
    case s^.undoStack of
        [] -> return s
        (actions:rest) -> do
            let go st [] old = return (st, old)
                go st (a:as) old = do
                    (st', old') <- applyAction st a
                    go st' as (old' <> old)

            (finalSt, undoActs) <- go s actions []
            return $ finalSt & undoStack .~ rest
                             & redoStack %~ (undoActs:)

redo :: AppState -> EventM Name AppState
redo s =
    case s^.redoStack of
        [] -> return s
        (actions:rest) -> do
            let go st [] old = return (st, old)
                go st (a:as) old = do
                    (st', old') <- applyAction st a
                    go st' as (old' <> old)

            (finalSt, undoActs) <- go s actions []
            return $ finalSt & redoStack .~ rest
                             & undoStack %~ (undoActs:)

applyAction :: AppState -> Action -> EventM Name (AppState, [Action])
applyAction s ClearCanvasDirty = return (s & canvasDirty .~ False, [])
applyAction s (SetPixels idx ps) = do
    let old' = (\(p, (ch, attr)) -> (p, ch, attr)) <$> ps
    (s', old) <- drawMany old' (layerAt idx) (Just idx) s
    return (s', old)
applyAction s (InsertLayer c idx ordIdx name) =
    return $ insertLayer c idx ordIdx name s
applyAction s (RemoveLayer idx) =
    return $ deleteLayer idx s
applyAction s (ChangeLayerName idx newName) =
    return $ renameLayer idx newName s
applyAction s (MoveLayerBy idx up) =
    return $ moveLayer idx up s
applyAction s (ToggleLayer idx) =
    return $ toggleLayer idx s

drawWithCurrentTool :: (Int, Int) -> AppState -> EventM Name AppState
drawWithCurrentTool point s =
    case s^.tool of
        Freehand -> drawAtPoint point s
        Eraser   -> eraseAtPoint point (s^.eraserSize) s
        Repaint  -> repaintAtPoint point (s^.repaintSize) s
        Restyle  -> restyleAtPoint point (s^.restyleSize) s
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
                            (s', _) <- drawBox bs l0 l1 drawingOverlay Nothing $
                                         s & drawingOverlay .~ o
                            return s'
                        _ -> return s
        Eyedropper ->
            -- Read the pixel at the canvas location. Set the
            -- application state's current drawing character and colors
            -- from it.
            let (ch, attr) = canvasGetPixel (s^.currentLayer) point
            in return $ s & drawCharacter .~ ch
                          & drawFgPaletteIndex .~ findFgPaletteEntry attr s
                          & drawBgPaletteIndex .~ findBgPaletteEntry attr s
                          & drawStyle .~ styleWord (V.attrStyle attr)

styleWord :: V.MaybeDefault V.Style -> V.Style
styleWord V.KeepCurrent = 0
styleWord V.Default = 0
styleWord (V.SetTo s) = s

truncateText :: (Int, Int) -> [(Char, V.Attr)] -> AppState -> [(Char, V.Attr)]
truncateText point t s =
    let startCol = point^._1
        maxCol = min ((s^.appCanvasSize)^._1 - 1)
                     (startCol + length t - 1)
        safe = take (maxCol - startCol + 1) t
    in safe

pasteTextAtPoint :: (Int, Int) -> AppState -> T.Text -> EventM Name AppState
pasteTextAtPoint point s t = do
    let ls = T.lines t
        (startCol, startRow) = point
        pasteWidth = maximum $ T.length <$> ls
        pasteHeight = length ls
        (oldWidth, oldHeight) = s^.appCanvasSize
        newSize = ( max oldWidth pasteWidth
                  , max oldHeight pasteHeight
                  )
        pairs = zip [startRow..] ls
        mkLine line = zip (T.unpack line) $ repeat $ currentPaletteAttribute s

    s' <- resizeCanvas s newSize
    foldM (\st (row, line) -> drawTextAtPoint (startCol, row) (mkLine line) st) s' pairs

drawTextAtPoint :: (Int, Int) -> [(Char, V.Attr)] -> AppState -> EventM Name AppState
drawTextAtPoint point t s = do
    let (startCol, row) = point
        pixs = zip ([startCol..]) (truncateText point t s)
        many = mkEntry <$> pixs
        mkEntry (col, (ch, attr)) = ((col, row), ch, attr)
    withUndo <$> drawMany many currentLayer (Just $ s^.selectedLayerIndex) s

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
    let fillAttr = normalizeAttr fillCh $ currentPaletteAttribute s
        fillCh = s^.drawCharacter
        fillPix = (fillCh, fillAttr)
        targetPix = canvasGetPixel (s^.currentLayer) point
        (w, h) = s^.appCanvasSize
        up    = (& _2 %~ (max 0 . pred))
        down  = (& _2 %~ (min (h-1) . succ))
        left  = (& _1 %~ (max 0 . pred))
        right = (& _1 %~ (min (w-1) . succ))

        go :: (Int, Int)
           -> (AppState, [((Int, Int), (Char, V.Attr))])
           -> EventM Name (AppState, [((Int, Int), (Char, V.Attr))])
        go p (st, uBuf) = do
            let pix = canvasGetPixel (st^.currentLayer) p
            if | pix == fillPix -> return (st, uBuf)
               | pix /= targetPix -> return (st, uBuf)
               | otherwise -> do
                   let old = canvasGetPixel (st^.currentLayer) p
                   d' <- liftIO $ canvasSetPixel (st^.currentLayer) p fillCh fillAttr
                   let newSt = st & currentLayer .~ d' & canvasDirty .~ True
                   go (down p) (newSt, (p, old):uBuf) >>=
                       go (up p) >>=
                       go (left p) >>=
                       go (right p)

    (finalSt, undoBuf) <- go point (s, [])
    let prevDirty = s^.canvasDirty
        newDirty = s^.canvasDirty
        d = if prevDirty /= newDirty
            then [ClearCanvasDirty]
            else []
    return $ pushUndo (d <> [SetPixels (s^.selectedLayerIndex) undoBuf]) finalSt

drawAtPoint :: (Int, Int) -> AppState -> EventM Name AppState
drawAtPoint point s =
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s) s

drawAtPoint' :: (Int, Int) -> Char -> V.Attr -> AppState -> EventM Name AppState
drawAtPoint' point ch attr s = do
    withUndo <$> drawMany [(point, ch, attr)] currentLayer (Just $ s^.selectedLayerIndex) s

drawMany :: [((Int, Int), Char, V.Attr)]
         -> Lens' AppState Canvas
         -> Maybe Int
         -> AppState
         -> EventM Name (AppState, [Action])
drawMany pixels which whichIdx s = do
    let arr = s^.which
        old = getOld <$> pixels
        getOld (oldLoc, _, _) = (oldLoc, canvasGetPixel (s^.which) oldLoc)
    arr' <- liftIO $ canvasSetMany arr pixels
    let prevDirty = s^.canvasDirty
        newDirty = not $ null pixels
        newSt = s & which .~ arr'
                  & canvasDirty .~ (prevDirty || newDirty)
    return (newSt, catMaybes [ do i <- whichIdx
                                  return $ SetPixels i old
                             , if prevDirty /= newDirty
                               then Just ClearCanvasDirty
                               else Nothing
                             ])

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
    withUndo <$> drawMany pixels currentLayer (Just $ s^.selectedLayerIndex) s

repaintAtPoint :: (Int, Int) -> Int -> AppState -> EventM Name AppState
repaintAtPoint point sz s = do
    let points = makeBoxAboutPoint point sz
        attr = currentPaletteAttribute s
        getPixel p = let old = canvasGetPixel (s^.currentLayer) p
                     in (p, old^._1, attr { V.attrStyle = V.attrStyle $ old^._2 })
        pixels = getPixel <$> points
    withUndo <$> drawMany pixels currentLayer (Just $ s^.selectedLayerIndex) s

restyleAtPoint :: (Int, Int) -> Int -> AppState -> EventM Name AppState
restyleAtPoint point sz s = do
    let points = makeBoxAboutPoint point sz
        attr = currentPaletteAttribute s
        getPixel p = let old = canvasGetPixel (s^.currentLayer) p
                     in (p, old^._1, (old^._2) { V.attrStyle = V.attrStyle attr })
        pixels = getPixel <$> points
    withUndo <$> drawMany pixels currentLayer (Just $ s^.selectedLayerIndex) s

drawBox :: BorderStyle
        -> Location
        -> Location
        -> Lens' AppState Canvas
        -> Maybe Int
        -> AppState
        -> EventM Name (AppState, [Action])
drawBox bs a b which whichIdx s = do
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

    drawMany pixels which whichIdx s

boxCorners :: Location -> Location -> ((Int, Int), (Int, Int))
boxCorners (Location (a0, a1)) (Location (b0, b1)) =
    let ul = (min a0 b0, min a1 b1)
        lr = (max a0 b0, max a1 b1)
    in (ul, lr)
