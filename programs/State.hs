{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module State
  ( checkForMouseSupport
  , setTool
  , setFgPaletteIndex
  , setBgPaletteIndex
  , beginFgPaletteSelect
  , beginBgPaletteSelect
  , beginToolSelect
  , selectNextLayer
  , selectPrevLayer
  , pushMode
  , popMode
  , increaseCanvasSize
  , decreaseCanvasSize
  , beginCanvasSizePrompt
  , toggleCurrentLayer
  , toggleLayer
  , beginTextEntry
  , tryResizeCanvas
  , quit
  , beginLayerRename
  , renameCurrentLayer
  , renameLayer
  , deleteLayer
  , deleteSelectedLayer
  , insertLayer
  , currentPaletteAttribute
  , handleDragFinished
  , getBoxBorderStyle
  , beginBoxStyleSelect
  , beginStyleSelect
  , writeCanvasFiles
  , resizeCanvas
  , increaseEraserSize
  , decreaseEraserSize
  , increaseRepaintSize
  , decreaseRepaintSize
  , increaseRestyleSize
  , decreaseRestyleSize
  , pushUndo
  , withUndo
  , toggleStyleFromKey
  , isStyleKey
  , styleBindings
  , recenterCanvas
  , addLayer
  , moveLayer
  , moveCurrentLayerDown
  , moveCurrentLayerUp

  , canvasMoveDown
  , canvasMoveUp
  , canvasMoveLeft
  , canvasMoveRight

  , tools
  , boxStyles

  , beginCharacterSelect
  , cancelCharacterSelect
  , selectCharacter
  )
where

import Control.Monad (when, forM)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sortOn, elemIndex)
import System.Exit (exitFailure)
import Lens.Micro.Platform
import Data.Text.Zipper (gotoEOL, textZipper)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust, catMaybes)

import Brick
import Brick.Focus
import Brick.Widgets.Edit (editor, applyEdit, getEditContents, editContentsL)
import Brick.Widgets.Border.Style

import Types
import Tart.Canvas
import Tart.Format

tools :: [(Tool, Int)]
tools =
    [ (Freehand  , 1)
    , (Box       , 2)
    , (FloodFill , 3)
    , (TextString, 4)
    , (Repaint   , 5)
    , (Restyle   , 6)
    , (Eyedropper, 7)
    , (Eraser    , 0)
    ]

styleBindings :: [(Char, V.Style)]
styleBindings =
    [ ('!', V.bold)
    , ('@', V.underline)
    , ('#', V.blink)
    , ('$', V.reverseVideo)
    ]

isStyleKey :: V.Event -> Bool
isStyleKey (V.EvKey (V.KChar c) []) =
    isJust $ lookup c styleBindings
isStyleKey _ = False

toggleStyleFromKey :: V.Event -> AppState -> AppState
toggleStyleFromKey e s =
    if isStyleKey e
    then let V.EvKey (V.KChar c) _ = e
             Just sty = lookup c styleBindings
         in s & drawStyle %~ toggleStyle sty
    else s

boxStyles :: [(String, BorderStyle)]
boxStyles =
    [ ("ASCII", ascii)
    , ("Unicode", unicode)
    , ("Unicode rounded", unicodeRounded)
    ]

getBoxBorderStyle :: AppState -> (String, BorderStyle)
getBoxBorderStyle s = boxStyles !! (s^.boxStyleIndex)

increaseEraserSize :: AppState -> AppState
increaseEraserSize = (& eraserSize %~ succ)

decreaseEraserSize :: AppState -> AppState
decreaseEraserSize = (& eraserSize %~ (max 1 . pred))

increaseRepaintSize :: AppState -> AppState
increaseRepaintSize = (& repaintSize %~ succ)

decreaseRepaintSize :: AppState -> AppState
decreaseRepaintSize = (& repaintSize %~ (max 1 . pred))

increaseRestyleSize :: AppState -> AppState
increaseRestyleSize = (& restyleSize %~ succ)

decreaseRestyleSize :: AppState -> AppState
decreaseRestyleSize = (& restyleSize %~ (max 1 . pred))

withUndo :: (AppState, [Action]) -> AppState
withUndo (s, as) = pushUndo as s

pushUndo :: [Action] -> AppState -> AppState
pushUndo [] s = s
pushUndo l s = s & undoStack %~ (l:)
                 & redoStack .~ []

beginLayerRename :: AppState -> AppState
beginLayerRename s =
    let z = textZipper [line] (Just 1)
        line = T.pack $ fromJust $ s^.layerNames.at (s^.selectedLayerIndex)
    in pushMode RenameLayer $
        s & layerNameEditor.editContentsL .~ gotoEOL z

toggleCurrentLayer :: AppState -> AppState
toggleCurrentLayer s =
    withUndo $ toggleLayer (s^.selectedLayerIndex) s

toggleLayer :: Int -> AppState -> (AppState, [Action])
toggleLayer idx s =
    ( s & layerVisible.ix idx %~ not
    , [ToggleLayer idx]
    )

renameCurrentLayer :: T.Text -> AppState -> AppState
renameCurrentLayer name s =
    withUndo $ renameLayer (s^.selectedLayerIndex) name s

renameLayer :: Int -> T.Text -> AppState -> (AppState, [Action])
renameLayer idx name s =
    let newName = T.unpack name
        Just oldName = s^.layerNames.at idx
        act = ChangeLayerName idx (T.pack oldName)
    in if null newName
       then (s, [])
       else if newName == oldName
            then (popMode s, [])
            else (popMode $
                   s & layerNames.at idx .~ Just (T.unpack name)
                     & canvasDirty .~ True
                 , [act])

moveCurrentLayerDown :: AppState -> AppState
moveCurrentLayerDown s =
    withUndo $ moveLayer (s^.selectedLayerIndex) False s

moveCurrentLayerUp :: AppState -> AppState
moveCurrentLayerUp s =
    withUndo $ moveLayer (s^.selectedLayerIndex) True s

moveLayer :: Int -> Bool -> AppState -> (AppState, [Action])
moveLayer idx up s =
    if up && idx == (head $ s^.layerOrder)
    then (s, [])
    else if (not up) && idx == (last $ s^.layerOrder)
         then (s, [])
         else let Just orderIndex = elemIndex idx $ s^.layerOrder
                  newIndex = if up then orderIndex - 1
                                   else orderIndex + 1
                  dropped = filter (/= idx) $ s^.layerOrder
                  newOrder = take newIndex dropped <>
                             [idx] <>
                             drop newIndex dropped
                  act = MoveLayerBy idx (not up)
              in (s & canvasDirty .~ True & layerOrder .~ newOrder, [act])

selectNextLayer :: AppState -> AppState
selectNextLayer s =
    -- Find the selected layer in the layer ordering.
    let Just selIndex = elemIndex (s^.selectedLayerIndex) (s^.layerOrder)
    -- Then select the next layer, if any.
        newSel = if selIndex == length (s^.layerOrder) - 1
                 then s^.selectedLayerIndex
                 else (s^.layerOrder) !! (selIndex + 1)
    in s & selectedLayerIndex .~ newSel

selectPrevLayer :: AppState -> AppState
selectPrevLayer s =
    -- Find the selected layer in the layer ordering.
    let Just selIndex = elemIndex (s^.selectedLayerIndex) (s^.layerOrder)
    -- Then select the previous layer, if any.
        newSel = if selIndex == 0
                 then s^.selectedLayerIndex
                 else (s^.layerOrder) !! (selIndex - 1)
    in s & selectedLayerIndex .~ newSel

deleteSelectedLayer :: AppState -> AppState
deleteSelectedLayer s =
    withUndo $ deleteLayer (s^.selectedLayerIndex) s

deleteLayer :: Int -> AppState -> (AppState, [Action])
deleteLayer idx s
    | M.size (s^.layers) == 1 = (s, [])
    | otherwise =
        let Just orderIndex = elemIndex idx (s^.layerOrder)
            Just selOrderIndex = elemIndex (s^.selectedLayerIndex) (s^.layerOrder)
            newSelIndex = if selOrderIndex == orderIndex
                          then newOrder !! (min (length newOrder - 1) selOrderIndex)
                          else s^.selectedLayerIndex
            newOrder = catMaybes $ fixOrder <$> s^.layerOrder
            fixOrder i = if idx == i
                         then Nothing
                         else Just $ if i > idx
                                     then i - 1
                                     else i

            fixNameKeys m = M.fromList $ catMaybes $ fixPair <$> M.toList m
            fixPair (i, n) = if idx == i
                             then Nothing
                             else (, n) <$> fixOrder i

            act = InsertLayer (s^.layerAt idx)
                              idx
                              orderIndex
                              (fromJust $ s^.layerNames.at idx)

        in (-- Change the selected index
           s & selectedLayerIndex .~ newSelIndex
             -- Remove the layer from the layer map, fix indices
             & layers %~ fixNameKeys
             -- Remove the layer from the layer visibility map, fix
             -- indices
             & layerVisible %~ fixNameKeys
             -- Reassign all higher indices in name map, ordering list,
             -- layer map
             & layerOrder .~ newOrder
             -- Remove the name, fix indices
             & layerNames %~ fixNameKeys
           , [act])

insertLayer :: Canvas -> Int -> Int -> String -> AppState -> (AppState, [Action])
insertLayer c newIdx orderIndex name s =
    let selIdx = s^.selectedLayerIndex
        newSelIndex = if selIdx >= newIdx
                      then selIdx + 1
                      else selIdx
        newOrderNoInsert = (\i -> if i >= newIdx then i + 1 else i) <$> s^.layerOrder
        newOrder = take orderIndex newOrderNoInsert <>
                   [newIdx] <>
                   drop orderIndex newOrderNoInsert

        fixNameKeys m = M.fromList $ fixPair <$> M.toList m
        fixPair (i, n) = if i >= newIdx
                         then (i + 1, n)
                         else (i, n)

        act = RemoveLayer newIdx

    in (
       s & selectedLayerIndex .~ newSelIndex
         & layers %~ (M.insert newIdx c . fixNameKeys)
         & layerVisible %~ (M.insert newIdx True . fixNameKeys)
         & layerOrder .~ newOrder
         & layerNames %~ (M.insert newIdx name . fixNameKeys)
       , [act])

quit :: Bool -> AppState -> EventM Name (Next AppState)
quit ask s = do
    case (s^.canvasDirty) of
        True ->
            case s^.canvasPath of
                Nothing ->
                    case ask of
                        True -> continue $ askToSave s
                        False -> halt s
                Just p ->
                    if ask
                    then continue $ askToSave s
                    else do
                        let ls = snd <$> (sortOn fst $ M.toList $ s^.layers)
                        liftIO $ writeCanvasFiles p ls (s^.layerOrder)
                                    (snd <$> (sortOn fst $ M.toList $ s^.layerNames))
                        halt s
        False -> halt s

writeCanvasFiles :: FilePath -> [Canvas] -> [Int] -> [String] -> IO ()
writeCanvasFiles path cs order names = do
    let tf = TartFile cs names order
    writeTartFile FormatBinary    tf path
    writeTartFile FormatPlain     tf (path <> ".plain.txt")
    writeTartFile FormatAnsiColor tf (path <> ".color.txt")

askToSave :: AppState -> AppState
askToSave s =
    pushMode AskToSave $
        s & askToSaveFilenameEdit .~ applyEdit gotoEOL (editor AskToSaveFilenameEdit (Just 1) $
                                           T.pack $ maybe "" id $ s^.canvasPath)

beginTextEntry :: (Int, Int) -> AppState -> AppState
beginTextEntry start s =
    pushMode TextEntry $ s & textEntryStart .~ start
                           & textEntered .~ mempty

handleDragFinished :: AppState -> Name -> EventM Name AppState
handleDragFinished s n =
    case n of
        Canvas ->
            case s^.tool of
                Box -> do
                    (c', old) <- liftIO $ merge (s^.currentLayer) (s^.drawingOverlay)
                    o' <- liftIO $ clearCanvas (s^.drawingOverlay)
                    return $ pushUndo [SetPixels (s^.selectedLayerIndex) old] $
                             s & currentLayer .~ c'
                               & drawingOverlay .~ o'
                _ -> return s
        _ -> return s

increaseCanvasSize :: AppState -> EventM Name AppState
increaseCanvasSize s =
    resizeCanvas s $
        (s^.appCanvasSize) & _1 %~ (\w -> if w == 1 then 4 else w + 4)
                           & _2 %~ (\h -> if h == 1 then 2 else h + 2)

decreaseCanvasSize :: AppState -> EventM Name AppState
decreaseCanvasSize s =
    resizeCanvas s $
        (s^.appCanvasSize) & _1 %~ (max 1 . (subtract 4))
                           & _2 %~ (max 1 . (subtract 2))

pushMode :: Mode -> AppState -> AppState
pushMode m s =
    if isSelectionMode m && isSelectionMode (currentMode s)
    then s & modes %~ ((m:) . tail)
           & dragging .~ Nothing
    else s & modes %~ (m:)
           & dragging .~ Nothing

popMode :: AppState -> AppState
popMode s = s & modes %~ (\m -> if length m == 1 then m else tail m)
              & dragging .~ Nothing

beginCanvasSizePrompt :: AppState -> AppState
beginCanvasSizePrompt s =
    pushMode CanvasSizePrompt $
        s & canvasSizeFocus .~ focusRing [ CanvasSizeWidthEdit
                                         , CanvasSizeHeightEdit
                                         ]
          & canvasSizeWidthEdit  .~ applyEdit gotoEOL (editor CanvasSizeWidthEdit (Just 1) $
                                           T.pack $ show $ fst $ s^.appCanvasSize)
          & canvasSizeHeightEdit .~ applyEdit gotoEOL (editor CanvasSizeHeightEdit (Just 1) $
                                           T.pack $ show $ snd $ s^.appCanvasSize)

canvasMoveDown :: AppState -> AppState
canvasMoveDown s =
    s & canvasOffset._2 %~ pred

canvasMoveUp :: AppState -> AppState
canvasMoveUp s =
    s & canvasOffset._2 %~ succ

canvasMoveLeft :: AppState -> AppState
canvasMoveLeft s =
    s & canvasOffset._1 %~ pred

canvasMoveRight :: AppState -> AppState
canvasMoveRight s =
    s & canvasOffset._1 %~ succ

tryResizeCanvas :: AppState -> EventM Name AppState
tryResizeCanvas s = do
    -- If the canvas size prompt inputs are valid, resize the canvas and
    -- exit prompt mode. Otherwise stay in prompt mode.
    let [wStr] = getEditContents $ s^.canvasSizeWidthEdit
        [hStr] = getEditContents $ s^.canvasSizeHeightEdit
        result = (,) <$> (readMaybe $ T.unpack wStr)
                     <*> (readMaybe $ T.unpack hStr)
    case result of
        Just (w, h) | w > 0 && h > 0 -> do
            resizeCanvas (popMode s) (w, h)
        _ -> return s

beginToolSelect :: AppState -> AppState
beginToolSelect = pushMode ToolSelect

beginBoxStyleSelect :: AppState -> AppState
beginBoxStyleSelect = pushMode BoxStyleSelect

beginStyleSelect :: AppState -> AppState
beginStyleSelect = pushMode StyleSelect

beginFgPaletteSelect :: AppState -> AppState
beginFgPaletteSelect = pushMode FgPaletteEntrySelect

beginBgPaletteSelect :: AppState -> AppState
beginBgPaletteSelect = pushMode BgPaletteEntrySelect

setTool :: AppState -> Tool -> AppState
setTool s t = s & tool .~ t

setFgPaletteIndex :: AppState -> Int -> AppState
setFgPaletteIndex s i = popMode $ s & drawFgPaletteIndex .~ i

setBgPaletteIndex :: AppState -> Int -> AppState
setBgPaletteIndex s i = popMode $ s & drawBgPaletteIndex .~ i

beginCharacterSelect :: AppState -> AppState
beginCharacterSelect = pushMode CharacterSelect

cancelCharacterSelect :: AppState -> AppState
cancelCharacterSelect = popMode

selectCharacter :: Char -> AppState -> AppState
selectCharacter c s = popMode $ s & drawCharacter .~ c

checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- V.mkVty =<< V.standardIOConfig

    when (not $ V.supportsMode (V.outputIface vty) V.Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure

    V.shutdown vty

resizeCanvas :: AppState -> (Int, Int) -> EventM n AppState
resizeCanvas s newSz = do
    ls <- liftIO $ forM (M.toList $ s^.layers) $ \(idx, l) ->
        (idx,) <$> resizeFrom l newSz
    o <- liftIO $ resizeFrom (s^.drawingOverlay) newSz
    return $
        recenterCanvas $
            s & layers .~ (M.fromList ls)
              & drawingOverlay .~ o
              & appCanvasSize .~ newSz
              & canvasDirty .~ (s^.appCanvasSize /= newSz)

recenterCanvas :: AppState -> AppState
recenterCanvas s =
    let sz = s^.appCanvasSize
    in s & canvasOffset .~ (Location $ sz & each %~ (`div` 2))

addLayer :: AppState -> EventM Name AppState
addLayer s = do
    let layerName = "layer " <> (show $ idx + 1)
        idx = M.size $ s^.layers
        orderIndex = length (s^.layerOrder)

    c <- liftIO $ newCanvas (s^.appCanvasSize)
    return $ withUndo $ insertLayer c idx orderIndex layerName s

currentPaletteAttribute :: AppState -> V.Attr
currentPaletteAttribute s =
    let fgEntry = Vec.unsafeIndex (s^.palette) (s^.drawFgPaletteIndex)
        bgEntry = Vec.unsafeIndex (s^.palette) (s^.drawBgPaletteIndex)
        applyFg Nothing = id
        applyFg (Just c) = (`V.withForeColor` c)
        applyBg Nothing = id
        applyBg (Just c) = (`V.withBackColor` c)
    in (applyFg fgEntry $ applyBg bgEntry V.defAttr) `V.withStyle` (s^.drawStyle)
