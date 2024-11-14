module Events.Main
  ( handleMainEvent
  , handleAttrEvent
  )
where

import Brick
import Control.Monad (when, void)
import Data.Char (isDigit)
import Data.Maybe (isJust)
import qualified Graphics.Vty as V
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import Lens.Micro.Platform
import Data.Text.Encoding (decodeUtf8)

import Types
import Draw
import State
import Events.Common

handleMainEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainEvent e = do
    result <- handleCommonEvent e
    case result of
        True -> return ()
        False -> do
            result2 <- handleAttrEvent e
            case result2 of
                True -> return ()
                False -> handleEvent e

handleAttrEvent :: BrickEvent Name AppEvent -> EventM Name AppState Bool
handleAttrEvent (MouseDown FgSelector _ _ _) = do
    beginFgPaletteSelect
    return True
handleAttrEvent (MouseDown BgSelector _ _ _) = do
    beginBgPaletteSelect
    return True
handleAttrEvent (MouseDown StyleSelector _ _ _) = do
    beginStyleSelect
    return True
handleAttrEvent _ =
    return False

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvPaste bytes)) =
    pasteTextAtPoint (0, 0) (decodeUtf8 bytes)
handleEvent (AppEvent (DragFinished n _ _)) =
    handleDragFinished n
handleEvent (VtyEvent (V.EvMouseDown _ _ V.BScrollUp _)) =
    increaseToolSize
handleEvent (VtyEvent (V.EvMouseDown _ _ V.BScrollDown _)) =
    decreaseToolSize
handleEvent (MouseDown _ V.BScrollUp _ _) =
    increaseToolSize
handleEvent (MouseDown _ V.BScrollDown _ _) =
    decreaseToolSize
handleEvent (MouseDown Canvas _ _ (Location l)) =
    drawWithCurrentTool l
handleEvent (MouseDown n _ _ _) =
    case n of
        LayerName           -> beginLayerRename
        DeleteLayer         -> deleteSelectedLayer
        MoveLayerUp         -> moveCurrentLayerUp
        MoveLayerDown       -> moveCurrentLayerDown
        ResizeCanvas        -> beginCanvasSizePrompt
        ToggleLayerVisible  -> toggleCurrentLayer
        ToolSelector        -> beginToolSelect
        IncreaseToolSize    -> increaseToolSize
        DecreaseToolSize    -> decreaseToolSize
        BoxStyleSelector    -> beginBoxStyleSelect
        SelectLayer idx     -> void $ selectLayer idx
        AddLayer            -> addLayer
        CharSelector        -> whenTool charTools beginCharacterSelect
        _                   -> return ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) =
    quit True
handleEvent (VtyEvent e) =
    case e of
        _ | isStyleKey e                       -> toggleStyleFromKey e
        (EvKey (KChar 'l') [MCtrl])            -> toggleLayerList
        (EvKey (KChar 'w') [])                 -> canvasMoveDown
        (EvKey (KChar 's') [])                 -> canvasMoveUp
        (EvKey (KChar 'a') [])                 -> canvasMoveLeft
        (EvKey (KChar 'd') [])                 -> canvasMoveRight
        (EvKey (KChar 'y') [])                 -> beginStyleSelect
        (EvKey (KChar 'v') [])                 -> beginCanvasSizePrompt
        (EvKey (KChar 's') [MCtrl])            -> askForSaveFilename False
        (EvKey (KChar 'r') [MCtrl])            -> beginLayerRename
        (EvKey (KChar 'x') [MCtrl])            -> deleteSelectedLayer
        (EvKey (KChar 'n') [MCtrl])            -> selectNextLayer
        (EvKey (KChar 'p') [MCtrl])            -> selectPrevLayer
        (EvKey (KChar 'u') [MCtrl])            -> moveCurrentLayerUp
        (EvKey (KChar 'd') [MCtrl])            -> moveCurrentLayerDown
        (EvKey (KChar 'v') [MCtrl])            -> toggleCurrentLayer
        (EvKey (KChar 'C') [])                 -> recenterCanvas
        (EvKey (KChar '>') [])                 -> increaseToolSize
        (EvKey (KChar '<') [])                 -> decreaseToolSize
        (EvKey KEsc [])                        -> do
                                                    drg <- use dragging
                                                    when (isJust drg) cancelDragging
        (EvKey (KChar c) []) | isDigit c       -> setToolByChar c
        (EvKey (KChar 'c') [])                 -> whenTool charTools beginCharacterSelect
        (EvKey (KChar '+') [])                 -> increaseCanvasSize
        (EvKey (KChar '-') [])                 -> decreaseCanvasSize
        (EvKey (KChar 'a') [MCtrl])            -> addLayer
        (EvKey (KChar 'u') [])                 -> undo
        (EvKey (KChar 'r') [])                 -> redo
        _                                      -> return ()
handleEvent _ = return ()
