{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Draw
  ( drawWithCurrentTool
  , drawAtPoint
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
import Control.Monad (foldM, void)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Data.Maybe (catMaybes)

import Types
import Tart.Canvas
import State
import Draw.Line
import Draw.Box

undo :: EventM Name AppState ()
undo = do
    s <- get
    case s^.undoStack of
        [] -> return ()
        (actions:rest) -> do
            let go [] old = return old
                go (a:as) old = do
                    old' <- applyAction a
                    go as (old' <> old)

            undoActs <- go actions []
            undoStack .= rest
            redoStack %= (undoActs:)

redo :: EventM Name AppState ()
redo = do
    s <- get
    case s^.redoStack of
        [] -> return ()
        (actions:rest) -> do
            let go [] old = return old
                go (a:as) old = do
                    old' <- applyAction a
                    go as (old' <> old)

            undoActs <- go actions []
            redoStack .= rest
            undoStack %= (undoActs:)

applyAction :: Action -> EventM Name AppState [Action]
applyAction ClearCanvasDirty = do
    canvasDirty .= False
    return []
applyAction (SetPixels idx ps) = do
    let old' = (\(p, (ch, attr)) -> (p, ch, attr)) <$> ps
    drawMany old' (layerAt idx) (Just idx)
applyAction (InsertLayer c idx ordIdx name) =
    insertLayer c idx ordIdx name
applyAction (RemoveLayer idx) =
    deleteLayer idx
applyAction (ChangeLayerName idx newName) =
    renameLayer idx newName
applyAction (MoveLayerBy idx up) =
    moveLayer idx up
applyAction (ToggleLayer idx) =
    toggleLayer idx
applyAction (SelectLayerIndex idx) =
    selectLayer idx

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

drawWithCurrentTool :: (Int, Int) -> EventM Name AppState ()
drawWithCurrentTool point = do
    s <- get
    case s^.tool of
        Freehand -> drawAtPoint point
        Eraser   -> eraseAtPoint point (s^.eraserSize)
        Repaint  -> repaintAtPoint point (s^.repaintSize)
        Restyle  -> restyleAtPoint point (s^.restyleSize)
        FloodFill -> floodFillAtPoint point
        TextString -> modify $ beginTextEntry point
        Line ->
            case s^.dragging of
                Nothing -> return ()
                Just (n, l0, l1) ->
                    case n of
                        Canvas -> do
                            o <- liftIO $ clearCanvas (s^.drawingOverlay)
                            drawingOverlay .= o
                            void $ drawLine l0 l1 drawingOverlay Nothing
                        _ -> return ()
        Box -> do
            case s^.dragging of
                Nothing -> return ()
                Just (n, l0, l1) ->
                    case n of
                        Canvas -> do
                            let bs = snd $ getBoxBorderStyle s
                            o <- liftIO $ clearCanvas (s^.drawingOverlay)
                            drawingOverlay .= o
                            void $ drawBox bs l0 l1 drawingOverlay Nothing
                        _ -> return ()
        Eyedropper -> do
            -- Read the pixel at the canvas location. Set the
            -- application state's current drawing character and colors
            -- from it.
            let (ch, attr) = canvasGetPixel (s^.currentLayer) point
            drawCharacter .= ch
            drawFgPaletteIndex .= findFgPaletteEntry attr s
            drawBgPaletteIndex .= findBgPaletteEntry attr s
            drawStyle .= styleWord (V.attrStyle attr)

styleWord :: V.MaybeDefault V.Style -> V.Style
styleWord V.KeepCurrent = 0
styleWord V.Default = 0
styleWord (V.SetTo s) = s

drawLine :: Location
         -> Location
         -> Lens' AppState Canvas
         -> Maybe Int
         -> EventM Name AppState [Action]
drawLine (Location p0) (Location p1) which whichIdx = do
    s <- get
    let points = plotLine p0 p1
        pixels = (, s^.drawCharacter, currentPaletteAttribute s) <$> points
    drawMany pixels which whichIdx

truncateText :: (Int, Int) -> [(Char, V.Attr)] -> AppState -> [(Char, V.Attr)]
truncateText point t s =
    let startCol = point^._1
        maxCol = min ((s^.appCanvasSize)^._1 - 1)
                     (startCol + length t - 1)
        safe = take (maxCol - startCol + 1) t
    in safe

pasteTextAtPoint :: (Int, Int) -> T.Text -> EventM Name AppState ()
pasteTextAtPoint point t = do
    s <- get
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

    resizeCanvas newSize
    mapM_ (\(row, line) -> drawTextAtPoint (startCol, row) (mkLine line)) pairs

drawTextAtPoint :: (Int, Int) -> [(Char, V.Attr)] -> EventM Name AppState ()
drawTextAtPoint point t = do
    s <- get
    let (startCol, row) = point
        pixs = zip ([startCol..]) (truncateText point t s)
        many = mkEntry <$> pixs
        mkEntry (col, (ch, attr)) = ((col, row), ch, attr)
    withUndoM $ drawMany many currentLayer (Just $ s^.selectedLayerIndex)

floodFillAtPoint :: (Int, Int) -> EventM Name AppState ()
floodFillAtPoint point = do
    s <- get

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
           -> [((Int, Int), (Char, V.Attr))]
           -> EventM Name AppState [((Int, Int), (Char, V.Attr))]
        go p uBuf = do
            curL <- use currentLayer
            let rawPix = canvasGetPixel curL p
                pix = rawPix & _2 %~ normalizeAttr (rawPix^._1)
            if | pix == fillPix -> return uBuf
               | pix /= targetPix -> return uBuf
               | otherwise -> do
                   let old = canvasGetPixel curL p
                   d' <- liftIO $ canvasSetPixel curL p fillCh fillAttr
                   currentLayer .= d'
                   canvasDirty .= True
                   go (down p) ((p, old):uBuf) >>=
                       go (up p) >>=
                       go (left p) >>=
                       go (right p)

    undoBuf <- go point []
    let prevDirty = s^.canvasDirty
        newDirty = s^.canvasDirty
        d = if prevDirty /= newDirty
            then [ClearCanvasDirty]
            else []
    modify $ pushUndo (d <> [SetPixels (s^.selectedLayerIndex) undoBuf])

drawAtPoint :: (Int, Int) -> EventM Name AppState ()
drawAtPoint point = do
    s <- get
    drawAtPoint' point (s^.drawCharacter) (currentPaletteAttribute s)

drawAtPoint' :: (Int, Int) -> Char -> V.Attr -> EventM Name AppState ()
drawAtPoint' point ch attr = do
    s <- get
    withUndoM $ drawMany [(point, ch, attr)] currentLayer (Just $ s^.selectedLayerIndex)

drawMany :: [((Int, Int), Char, V.Attr)]
         -> Lens' AppState Canvas
         -> Maybe Int
         -> EventM Name AppState [Action]
drawMany pixels which whichIdx = do
    s <- get
    let arr = s^.which
        old = getOld <$> pixels
        getOld (oldLoc, _, _) = (oldLoc, canvasGetPixel (s^.which) oldLoc)
    arr' <- liftIO $ canvasSetMany arr pixels
    let prevDirty = s^.canvasDirty
        newDirty = not $ null pixels
        newSt = s & which .~ arr'
                  & canvasDirty .~ (prevDirty || newDirty)
    put newSt
    return (catMaybes [ do i <- whichIdx
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

eraseAtPoint :: (Int, Int) -> Int -> EventM Name AppState ()
eraseAtPoint point sz = do
    s <- get
    let points = makeBoxAboutPoint point sz
        pixels = (, ' ', V.defAttr) <$> points
    withUndoM $ drawMany pixels currentLayer (Just $ s^.selectedLayerIndex)

repaintAtPoint :: (Int, Int) -> Int -> EventM Name AppState ()
repaintAtPoint point sz = do
    s <- get
    let points = makeBoxAboutPoint point sz
        attr = currentPaletteAttribute s
        getPixel p = let old = canvasGetPixel (s^.currentLayer) p
                     in (p, old^._1, attr { V.attrStyle = V.attrStyle $ old^._2 })
        pixels = getPixel <$> points
    withUndoM $ drawMany pixels currentLayer (Just $ s^.selectedLayerIndex)

restyleAtPoint :: (Int, Int) -> Int -> EventM Name AppState ()
restyleAtPoint point sz = do
    s <- get
    let points = makeBoxAboutPoint point sz
        attr = currentPaletteAttribute s
        getPixel p = let old = canvasGetPixel (s^.currentLayer) p
                     in (p, old^._1, (old^._2) { V.attrStyle = V.attrStyle attr })
        pixels = getPixel <$> points
    withUndoM $ drawMany pixels currentLayer (Just $ s^.selectedLayerIndex)

drawBox :: BorderStyle
        -> Location
        -> Location
        -> Lens' AppState Canvas
        -> Maybe Int
        -> EventM Name AppState [Action]
drawBox bs (Location a) (Location b) which whichIdx = do
    s <- get
    let attr = currentPaletteAttribute s
        points = plotBox bs a b
        pixels = mkPixel <$> points
        mkPixel (p, ch) = (p, ch, attr)
    drawMany pixels which whichIdx
