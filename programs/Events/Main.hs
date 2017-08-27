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
handleEvent s (MouseDown Canvas _ _ (Location l)) =
    continue =<< drawWithCurrentTool l s
handleEvent s (MouseDown _ V.BScrollUp _ _) =
    continue $ increaseToolSize s
handleEvent s (MouseDown _ V.BScrollDown _ _) =
    continue $ decreaseToolSize s
handleEvent s (MouseDown n _ _ _) =
    continue =<< case n of
        LayerName           -> return $ beginLayerRename s
        DeleteLayer         -> return $ deleteSelectedLayer s
        MoveLayerUp         -> return $ moveCurrentLayerUp s
        MoveLayerDown       -> return $ moveCurrentLayerDown s
        ResizeCanvas        -> return $ beginCanvasSizePrompt s
        ToggleLayerVisible  -> return $ toggleCurrentLayer s
        ToolSelector        -> return $ beginToolSelect s
        IncreaseEraserSize  -> return $ increaseEraserSize s
        DecreaseEraserSize  -> return $ decreaseEraserSize s
        IncreaseRepaintSize -> return $ increaseRepaintSize s
        DecreaseRepaintSize -> return $ decreaseRepaintSize s
        IncreaseRestyleSize -> return $ increaseRestyleSize s
        DecreaseRestyleSize -> return $ decreaseRestyleSize s
        BoxStyleSelector    -> return $ beginBoxStyleSelect s
        SelectLayer idx     -> return $ selectLayer idx s
        AddLayer            -> addLayer s
        CharSelector        -> return $ whenTool s [Freehand, FloodFill] beginCharacterSelect
        _                   -> return s
handleEvent s (VtyEvent e) | isStyleKey e =
    continue $ toggleStyleFromKey e s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'w') [])) =
    continue $ canvasMoveDown s
handleEvent s (VtyEvent (V.EvKey (V.KChar 's') [])) =
    continue $ canvasMoveUp s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'a') [])) =
    continue $ canvasMoveLeft s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [])) =
    continue $ canvasMoveRight s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'v') [])) =
    continue $ beginCanvasSizePrompt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) =
    continue $ beginLayerRename s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl])) =
    continue $ deleteSelectedLayer s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) =
    continue $ selectNextLayer s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl])) =
    continue $ selectPrevLayer s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl])) =
    continue $ moveCurrentLayerUp s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) =
    continue $ moveCurrentLayerDown s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'v') [V.MCtrl])) =
    continue $ toggleCurrentLayer s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'C') [])) =
    continue $ recenterCanvas s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'f') [])) =
    continue $ beginFgPaletteSelect s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) =
    continue $ beginBgPaletteSelect s
handleEvent s (VtyEvent (V.EvKey (V.KChar '>') [])) =
    continue $ increaseToolSize s
handleEvent s (VtyEvent (V.EvKey (V.KChar '<') [])) =
    continue $ decreaseToolSize s
handleEvent s (VtyEvent (V.EvKey (V.KChar '+') [])) =
    continue =<< increaseCanvasSize s
handleEvent s (VtyEvent (V.EvKey (V.KChar '-') [])) =
    continue =<< decreaseCanvasSize s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) | isJust (s^.dragging) =
    continue $ cancelDragging s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl])) =
    continue =<< addLayer s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) =
    quit True s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'u') [])) =
    continue =<< undo s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'r') [])) =
    continue =<< redo s
handleEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c =
    continue $ setToolByChar c s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) =
    continue $ whenTool s [Freehand, FloodFill] beginCharacterSelect
handleEvent s _ = continue s
