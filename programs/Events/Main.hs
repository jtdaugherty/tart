module Events.Main
  ( handleMainEvent
  , handleAttrEvent
  )
where

import Brick
import Data.Char (isDigit)
import Data.Maybe (isJust)
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Data.Text.Encoding (decodeUtf8)

import Types
import Draw
import State
import Events.Common

handleMainEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleMainEvent s e = do
    result <- handleCommonEvent s e
    case result of
        Just s' -> continue s'
        Nothing -> do
            result2 <- handleAttrEvent s e
            case result2 of
                Just s'' -> continue s''
                Nothing -> handleEvent s e

handleAttrEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Maybe AppState)
handleAttrEvent s (MouseDown FgSelector _ _ _) =
    return $ Just $ beginFgPaletteSelect s
handleAttrEvent s (MouseDown BgSelector _ _ _) =
    return $ Just $ beginBgPaletteSelect s
handleAttrEvent s (MouseDown StyleSelector _ _ _) =
    return $ Just $ beginStyleSelect s
handleAttrEvent _ _ = return Nothing

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvPaste bytes)) =
    continue =<< pasteTextAtPoint (0, 0) s (decodeUtf8 bytes)
handleEvent s (AppEvent (DragFinished n _ _)) =
    continue =<< handleDragFinished s n
handleEvent s (VtyEvent (V.EvMouseDown _ _ V.BScrollUp _)) =
    continue $ increaseToolSize s
handleEvent s (VtyEvent (V.EvMouseDown _ _ V.BScrollDown _)) =
    continue $ decreaseToolSize s
handleEvent s (MouseDown _ V.BScrollUp _ _) =
    continue $ increaseToolSize s
handleEvent s (MouseDown _ V.BScrollDown _ _) =
    continue $ decreaseToolSize s
handleEvent s (MouseDown Canvas _ _ (Location l)) =
    continue =<< drawWithCurrentTool l s
handleEvent s (MouseDown n _ _ _) =
    continue =<< case n of
        LayerName           -> return $ beginLayerRename s
        DeleteLayer         -> return $ deleteSelectedLayer s
        MoveLayerUp         -> return $ moveCurrentLayerUp s
        MoveLayerDown       -> return $ moveCurrentLayerDown s
        ResizeCanvas        -> return $ beginCanvasSizePrompt s
        ToggleLayerVisible  -> return $ toggleCurrentLayer s
        ToolSelector        -> return $ beginToolSelect s
        IncreaseToolSize    -> return $ increaseToolSize s
        DecreaseToolSize    -> return $ decreaseToolSize s
        BoxStyleSelector    -> return $ beginBoxStyleSelect s
        SelectLayer idx     -> return $ selectLayer idx s
        AddLayer            -> addLayer s
        CharSelector        -> return $ whenTool s [Freehand, FloodFill] beginCharacterSelect
        _                   -> return s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) =
    quit True s
handleEvent s (VtyEvent e) =
    continue =<< case e of
        _ | isStyleKey e -> return $ toggleStyleFromKey e s
        (V.EvKey (V.KChar 'l') [V.MCtrl]) -> return $ toggleLayerList s
        (V.EvKey (V.KChar 'w') []) -> return $ canvasMoveDown s
        (V.EvKey (V.KChar 's') []) -> return $ canvasMoveUp s
        (V.EvKey (V.KChar 'a') []) -> return $ canvasMoveLeft s
        (V.EvKey (V.KChar 'd') []) -> return $ canvasMoveRight s
        (V.EvKey (V.KChar 'y') []) -> return $ beginStyleSelect s
        (V.EvKey (V.KChar 'v') []) -> return $ beginCanvasSizePrompt s
        (V.EvKey (V.KChar 'r') [V.MCtrl]) -> return $ beginLayerRename s
        (V.EvKey (V.KChar 'x') [V.MCtrl]) -> return $ deleteSelectedLayer s
        (V.EvKey (V.KChar 'n') [V.MCtrl]) -> return $ selectNextLayer s
        (V.EvKey (V.KChar 'p') [V.MCtrl]) -> return $ selectPrevLayer s
        (V.EvKey (V.KChar 'u') [V.MCtrl]) -> return $ moveCurrentLayerUp s
        (V.EvKey (V.KChar 'd') [V.MCtrl]) -> return $ moveCurrentLayerDown s
        (V.EvKey (V.KChar 'v') [V.MCtrl]) -> return $ toggleCurrentLayer s
        (V.EvKey (V.KChar 'C') []) -> return $ recenterCanvas s
        (V.EvKey (V.KChar 'f') []) -> return $ beginFgPaletteSelect s
        (V.EvKey (V.KChar 'b') []) -> return $ beginBgPaletteSelect s
        (V.EvKey (V.KChar '>') []) -> return $ increaseToolSize s
        (V.EvKey (V.KChar '<') []) -> return $ decreaseToolSize s
        (V.EvKey V.KEsc []) | isJust (s^.dragging) -> return $ cancelDragging s
        (V.EvKey (V.KChar c) []) | isDigit c -> return $ setToolByChar c s
        (V.EvKey (V.KChar 'c') []) -> return $ whenTool s [Freehand, FloodFill]
                                               beginCharacterSelect
        (V.EvKey (V.KChar '+') []) -> increaseCanvasSize s
        (V.EvKey (V.KChar '-') []) -> decreaseCanvasSize s
        (V.EvKey (V.KChar 'a') [V.MCtrl]) -> addLayer s
        (V.EvKey (V.KChar 'u') []) -> undo s
        (V.EvKey (V.KChar 'r') []) -> redo s
        _ -> return s
handleEvent s _ = continue s
