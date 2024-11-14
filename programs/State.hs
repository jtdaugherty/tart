{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module State
  ( checkForMouseSupport
  , setTool
  , setToolByChar
  , whenTool
  , setFgPaletteIndex
  , setBgPaletteIndex
  , beginFgPaletteSelect
  , beginBgPaletteSelect
  , beginToolSelect
  , selectNextLayer
  , selectPrevLayer
  , selectLayer
  , pushMode
  , popMode
  , toolSize
  , askForSaveFilename
  , increaseCanvasSize
  , decreaseCanvasSize
  , increaseToolSize
  , decreaseToolSize
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
  , withUndoM
  , toggleStyleFromKey
  , isStyleKey
  , styleBindings
  , recenterCanvas
  , addLayer
  , moveLayer
  , moveCurrentLayerDown
  , moveCurrentLayerUp
  , cancelDragging
  , toggleLayerList
  , saveAndContinue

  , canvasMoveDown
  , canvasMoveUp
  , canvasMoveLeft
  , canvasMoveRight

  , tools
  , charTools
  , boxStyles

  , beginCharacterSelect
  , cancelCharacterSelect
  , selectCharacter
  )
where

import Control.Monad (when, forM, forM_)
import Control.Monad.Trans (liftIO)
import qualified Control.Exception as E
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
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
    , (Line      , 3)
    , (FloodFill , 4)
    , (TextString, 5)
    , (Repaint   , 6)
    , (Restyle   , 7)
    , (Eyedropper, 8)
    , (Eraser    , 0)
    ]

charTools :: [Tool]
charTools =
    [ Freehand
    , Line
    , FloodFill
    ]

styleBindings :: [(Char, (V.Style, T.Text))]
styleBindings =
    [ ('!', (V.bold, "Bold"))
    , ('@', (V.underline, "Underline"))
    , ('#', (V.blink, "Blink"))
    , ('$', (V.reverseVideo, "Reverse"))
    ]

isStyleKey :: V.Event -> Bool
isStyleKey (V.EvKey (V.KChar c) []) =
    isJust $ lookup c styleBindings
isStyleKey _ = False

toggleStyleFromKey :: V.Event -> EventM Name AppState ()
toggleStyleFromKey e =
    when (isStyleKey e) $ do
        let V.EvKey (V.KChar c) _ = e
            Just (sty, _) = lookup c styleBindings
        drawStyle %= toggleStyle sty

boxStyles :: [(T.Text, BorderStyle)]
boxStyles =
    [ ("ASCII", ascii)
    , ("Unicode", unicode)
    , ("Unicode rounded", unicodeRounded)
    ]

getBoxBorderStyle :: AppState -> (T.Text, BorderStyle)
getBoxBorderStyle s = boxStyles !! (s^.boxStyleIndex)

increaseToolSize :: EventM Name AppState ()
increaseToolSize = do
    s <- get
    let f = case s^.tool of
              Repaint -> increaseRepaintSize
              Restyle -> increaseRestyleSize
              Eraser  -> increaseEraserSize
              _ -> id
    modify f

decreaseToolSize :: EventM Name AppState ()
decreaseToolSize = do
    s <- get
    let f = case s^.tool of
              Repaint -> decreaseRepaintSize
              Restyle -> decreaseRestyleSize
              Eraser  -> decreaseEraserSize
              _ -> id
    modify f

toolSize :: AppState -> Maybe Int
toolSize s =
    case s^.tool of
        Repaint -> Just $ s^.repaintSize
        Restyle -> Just $ s^.restyleSize
        Eraser  -> Just $ s^.eraserSize
        _ -> Nothing

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

withUndoM :: EventM Name AppState [Action] -> EventM Name AppState ()
withUndoM act = do
    as <- act
    modify $ pushUndo as

withUndo :: (AppState, [Action]) -> AppState
withUndo (s, as) = pushUndo as s

pushUndo :: [Action] -> AppState -> AppState
pushUndo [] s = s
pushUndo l s = s & undoStack %~ (l:)
                 & redoStack .~ []

beginLayerRename :: EventM Name AppState ()
beginLayerRename = do
    s <- get
    let z = textZipper [line] (Just 1)
        line = s^.layerInfoFor(s^.selectedLayerIndex).layerName
    layerNameEditor.editContentsL .= gotoEOL z
    modify $ pushMode RenameLayer

toggleCurrentLayer :: EventM Name AppState ()
toggleCurrentLayer = do
    idx <- use selectedLayerIndex
    withUndoM $ toggleLayer idx

toggleLayer :: Int -> EventM Name AppState [Action]
toggleLayer idx = do
    layerInfoFor(idx).layerVisible %= not
    return  [ToggleLayer idx]

renameCurrentLayer :: T.Text -> EventM Name AppState ()
renameCurrentLayer name = do
    idx <- use selectedLayerIndex
    withUndoM $ renameLayer idx name

renameLayer :: Int -> T.Text -> EventM Name AppState [Action]
renameLayer idx newName = do
    s <- get
    let oldName = s^.layerInfoFor(idx).layerName
        act = ChangeLayerName idx oldName
    if T.null newName
       then return []
       else if newName == oldName
            then do
                modify popMode >> return []
            else do
                modify popMode
                layerInfoFor(idx).layerName .= newName
                canvasDirty .= True
                return [act]

moveCurrentLayerDown :: EventM Name AppState ()
moveCurrentLayerDown = do
    idx <- use selectedLayerIndex
    withUndoM $ moveLayer idx False

moveCurrentLayerUp :: EventM Name AppState ()
moveCurrentLayerUp = do
    idx <- use selectedLayerIndex
    withUndoM $ moveLayer idx True

moveLayer :: Int -> Bool -> EventM Name AppState [Action]
moveLayer idx up = do
    s <- get
    if up && idx == (head $ s^.layerOrder)
    then return []
    else if (not up) && idx == (last $ s^.layerOrder)
         then return []
         else let Just orderIndex = elemIndex idx $ s^.layerOrder
                  newIndex = if up then orderIndex - 1
                                   else orderIndex + 1
                  dropped = filter (/= idx) $ s^.layerOrder
                  newOrder = take newIndex dropped <>
                             [idx] <>
                             drop newIndex dropped
                  act = MoveLayerBy idx (not up)
              in do
                  canvasDirty .= True
                  layerOrder .= newOrder
                  return [act]

selectNextLayer :: EventM Name AppState ()
selectNextLayer = do
    s <- get
    -- Find the selected layer in the layer ordering.
    let Just selIndex = elemIndex (s^.selectedLayerIndex) (s^.layerOrder)
    -- Then select the next layer, if any.
        newSel = if selIndex == length (s^.layerOrder) - 1
                 then s^.selectedLayerIndex
                 else (s^.layerOrder) !! (selIndex + 1)
    selectedLayerIndex .= newSel

selectPrevLayer :: EventM Name AppState ()
selectPrevLayer = do
    s <- get
    -- Find the selected layer in the layer ordering.
    let Just selIndex = elemIndex (s^.selectedLayerIndex) (s^.layerOrder)
    -- Then select the previous layer, if any.
        newSel = if selIndex == 0
                 then s^.selectedLayerIndex
                 else (s^.layerOrder) !! (selIndex - 1)
    selectedLayerIndex .= newSel

selectLayer :: Int -> EventM Name AppState [Action]
selectLayer idx = do
    oldIdx <- use selectedLayerIndex
    selectedLayerIndex .= idx
    return [SelectLayerIndex oldIdx]

cancelDragging :: EventM Name AppState ()
cancelDragging =
    dragging .= Nothing

deleteSelectedLayer :: EventM Name AppState ()
deleteSelectedLayer = do
    idx <- use selectedLayerIndex
    withUndoM $ deleteLayer idx

deleteLayer :: Int -> EventM Name AppState [Action]
deleteLayer idx = do
    s <- get
    if M.size (s^.layers) == 1
       then return []
       else do
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
                              (_layerName $ fromJust $ s^.layerInfo.at idx)

        -- Change the selected index
        selectedLayerIndex .= newSelIndex
        -- Remove the layer from the layer map, fix indices
        layers %= fixNameKeys
        -- Reassign all higher indices in name map, ordering list,
        -- layer map
        layerOrder .= newOrder
        -- Remove the layer from the layer visibility map, fix
        -- indices
        layerInfo %= fixNameKeys
        return [act]

insertLayer :: Canvas -> Int -> Int -> T.Text -> EventM Name AppState [Action]
insertLayer c newIdx orderIndex name = do
    s <- get
    let newOrderNoInsert = (\i -> if i >= newIdx then i + 1 else i) <$> s^.layerOrder
        newOrder = take orderIndex newOrderNoInsert <>
                   [newIdx] <>
                   drop orderIndex newOrderNoInsert

        fixNameKeys m = M.fromList $ fixPair <$> M.toList m
        fixPair (i, n) = if i >= newIdx
                         then (i + 1, n)
                         else (i, n)

        removeAct = RemoveLayer newIdx
        selAct = SelectLayerIndex (s^.selectedLayerIndex)

    selectedLayerIndex .= (length (s^.layerOrder))
    layers %= (M.insert newIdx c . fixNameKeys)
    layerOrder .= newOrder
    layerInfo %= (M.insert newIdx (LayerInfo name True) . fixNameKeys)
    return [removeAct, selAct]

quit :: Bool -> EventM Name AppState ()
quit ask = do
    s <- get
    case (s^.canvasDirty) of
        True ->
            case s^.canvasPath of
                Nothing ->
                    case ask of
                        True -> modify askToSave
                        False -> halt
                Just p ->
                    if ask
                    then modify askToSave
                    else do
                        result <- liftIO $ E.try $ saveToDisk s p
                        case result of
                            Left (e::E.SomeException) -> do
                                saveError .= (Just $ T.pack $ show e)
                                askForSaveFilename True
                            Right () -> halt
        False -> halt

saveToDisk :: AppState -> FilePath -> IO ()
saveToDisk s p = do
    let ls = snd <$> (sortOn fst $ M.toList $ s^.layers)
    writeCanvasFiles p ls (s^.layerOrder)
        (_layerName <$> snd <$> (sortOn fst $ M.toList $ s^.layerInfo))

saveAndContinue :: EventM Name AppState ()
saveAndContinue = do
    s <- get
    case s^.canvasPath of
        Nothing -> return ()
        Just p -> do
            liftIO $ saveToDisk s p
            put $ s & canvasDirty .~ False

writeCanvasFiles :: FilePath -> [Canvas] -> [Int] -> [T.Text] -> IO ()
writeCanvasFiles path cs order names = do
    let tf = TartFile cs names order
        tfp = toTartFilepath path
        formats = [FormatBinary, FormatPlain, FormatAnsiColor]
    forM_ formats $ \f -> writeTartFile f tf tfp

askToSave :: AppState -> AppState
askToSave s =
    pushMode AskToSave s

askForSaveFilename :: Bool -> EventM Name AppState ()
askForSaveFilename shouldQuit = do
    s <- get
    askToSaveFilenameEdit .= applyEdit gotoEOL (editor AskToSaveFilenameEdit (Just 1) $
                                     T.pack $ maybe "" id $ s^.canvasPath)
    modify $ pushMode (AskForSaveFilename shouldQuit)

beginTextEntry :: (Int, Int) -> AppState -> AppState
beginTextEntry start s =
    pushMode TextEntry $ s & textEntryStart .~ start
                           & textEntered .~ mempty

handleDragFinished :: Name -> EventM Name AppState ()
handleDragFinished n = do
    s <- get
    case n of
        Canvas ->
            case s^.tool `elem` [Box, Line] of
                True -> do
                    (c', old) <- liftIO $ merge (s^.currentLayer) (s^.drawingOverlay)
                    o' <- liftIO $ clearCanvas (s^.drawingOverlay)
                    put $ pushUndo [SetPixels (s^.selectedLayerIndex) old] $
                             s & currentLayer .~ c'
                               & drawingOverlay .~ o'
                False -> return ()
        _ -> return ()

increaseCanvasSize :: EventM Name AppState ()
increaseCanvasSize = do
    sz <- use appCanvasSize
    resizeCanvas $
        sz & _1 %~ (\w -> if w == 1 then 4 else w + 4)
           & _2 %~ (\h -> if h == 1 then 2 else h + 2)

decreaseCanvasSize :: EventM Name AppState ()
decreaseCanvasSize = do
    sz <- use appCanvasSize
    resizeCanvas $
        sz & _1 %~ (max 1 . (subtract 4))
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

beginCanvasSizePrompt :: EventM Name AppState ()
beginCanvasSizePrompt = do
    s <- get
    canvasSizeFocus .= focusRing [ CanvasSizeWidthEdit
                                 , CanvasSizeHeightEdit
                                 ]
    canvasSizeWidthEdit .= applyEdit gotoEOL (editor CanvasSizeWidthEdit (Just 1) $
                                   T.pack $ show $ fst $ s^.appCanvasSize)
    canvasSizeHeightEdit .= applyEdit gotoEOL (editor CanvasSizeHeightEdit (Just 1) $
                                   T.pack $ show $ snd $ s^.appCanvasSize)
    modify $ pushMode CanvasSizePrompt

canvasMoveDown :: EventM Name AppState ()
canvasMoveDown =
    canvasOffset._2 %= pred

canvasMoveUp :: EventM Name AppState ()
canvasMoveUp =
    canvasOffset._2 %= succ

canvasMoveLeft :: EventM Name AppState ()
canvasMoveLeft =
    canvasOffset._1 %= pred

canvasMoveRight :: EventM Name AppState ()
canvasMoveRight =
    canvasOffset._1 %= succ

tryResizeCanvas :: EventM Name AppState ()
tryResizeCanvas = do
    s <- get
    -- If the canvas size prompt inputs are valid, resize the canvas and
    -- exit prompt mode. Otherwise stay in prompt mode.
    let [wStr] = getEditContents $ s^.canvasSizeWidthEdit
        [hStr] = getEditContents $ s^.canvasSizeHeightEdit
        result = (,) <$> (readMaybe $ T.unpack wStr)
                     <*> (readMaybe $ T.unpack hStr)
    case result of
        Just (w, h) | w > 0 && h > 0 -> do
            modify popMode
            resizeCanvas (w, h)
        _ -> return ()

beginToolSelect :: EventM Name AppState ()
beginToolSelect = modify $ pushMode ToolSelect

beginBoxStyleSelect :: EventM Name AppState ()
beginBoxStyleSelect = modify $ pushMode BoxStyleSelect

beginStyleSelect :: EventM Name AppState ()
beginStyleSelect = modify $ pushMode StyleSelect

beginFgPaletteSelect :: EventM Name AppState ()
beginFgPaletteSelect = modify $ pushMode FgPaletteEntrySelect

beginBgPaletteSelect :: EventM Name AppState ()
beginBgPaletteSelect = modify $ pushMode BgPaletteEntrySelect

setTool :: Tool -> EventM Name AppState ()
setTool t = tool .= t

setToolByChar :: Char -> EventM Name AppState ()
setToolByChar c =
    let idx = read [c]
    in case filter ((== idx) . snd) tools of
        [(t, _)] -> do
            setTool t
            modify popMode
        _ -> return ()

whenTool :: [Tool] -> EventM Name AppState () -> EventM Name AppState ()
whenTool ts act = do
    t <- use tool
    when (t `elem` ts) act

setFgPaletteIndex :: Int -> EventM Name AppState ()
setFgPaletteIndex i = do
    drawFgPaletteIndex .= i
    modify popMode

setBgPaletteIndex :: Int -> EventM Name AppState ()
setBgPaletteIndex i = do
    drawBgPaletteIndex .= i
    modify popMode

beginCharacterSelect :: EventM Name AppState ()
beginCharacterSelect = modify $ pushMode CharacterSelect

cancelCharacterSelect :: EventM Name AppState ()
cancelCharacterSelect = modify popMode

selectCharacter :: Char -> EventM Name AppState ()
selectCharacter c = do
    drawCharacter .= c
    modify popMode

checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- V.mkVty V.defaultConfig

    when (not $ V.supportsMode (V.outputIface vty) V.Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure

    V.shutdown vty

resizeCanvas :: (Int, Int) -> EventM n AppState ()
resizeCanvas newSz = do
    s <- get
    ls <- liftIO $ forM (M.toList $ s^.layers) $ \(idx, l) ->
        (idx,) <$> resizeFrom l newSz
    o <- liftIO $ resizeFrom (s^.drawingOverlay) newSz
    layers .= (M.fromList ls)
    drawingOverlay .= o
    appCanvasSize .= newSz
    canvasDirty .= (s^.appCanvasSize /= newSz)
    recenterCanvas

recenterCanvas :: EventM n AppState ()
recenterCanvas = do
    sz <- use appCanvasSize
    canvasOffset .= (Location $ sz & each %~ (`div` 2))

toggleLayerList :: EventM Name AppState ()
toggleLayerList =
    layerListVisible %= not

addLayer :: EventM Name AppState ()
addLayer = do
    s <- get
    let newLayerName = T.pack $ "layer " <> (show $ idx + 1)
        idx = M.size $ s^.layers
        orderIndex = length (s^.layerOrder)

    c <- liftIO $ newCanvas (s^.appCanvasSize)
    withUndoM $ insertLayer c idx orderIndex newLayerName

currentPaletteAttribute :: AppState -> V.Attr
currentPaletteAttribute s =
    let fgEntry = Vec.unsafeIndex (s^.palette) (s^.drawFgPaletteIndex)
        bgEntry = Vec.unsafeIndex (s^.palette) (s^.drawBgPaletteIndex)
        applyFg Nothing = id
        applyFg (Just c) = (`V.withForeColor` c)
        applyBg Nothing = id
        applyBg (Just c) = (`V.withBackColor` c)
    in (applyFg fgEntry $ applyBg bgEntry V.defAttr) `V.withStyle` (s^.drawStyle)
