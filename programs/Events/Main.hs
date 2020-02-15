module Events.Main
  ( handleMainEvent
  , handleAttrEvent
  )
where

import Brick
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
        CharSelector        -> return $ whenTool s charTools beginCharacterSelect
        _                   -> return s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) =
    quit True s
handleEvent s (VtyEvent e) =
    continue =<< case e of
        _ | isStyleKey e                       -> return $ toggleStyleFromKey e s
        (EvKey (KChar 'l') [MCtrl])            -> return $ toggleLayerList s
        (EvKey (KChar 'w') [])                 -> return $ canvasMoveDown s
        (EvKey (KChar 's') [])                 -> return $ canvasMoveUp s
        (EvKey (KChar 'a') [])                 -> return $ canvasMoveLeft s
        (EvKey (KChar 'd') [])                 -> return $ canvasMoveRight s
        (EvKey (KChar 'y') [])                 -> return $ beginStyleSelect s
        (EvKey (KChar 'v') [])                 -> return $ beginCanvasSizePrompt s
        (EvKey (KChar 's') [MCtrl])            -> return $ askForSaveFilename False s
        (EvKey (KChar 'r') [MCtrl])            -> return $ beginLayerRename s
        (EvKey (KChar 'x') [MCtrl])            -> return $ deleteSelectedLayer s
        (EvKey (KChar 'n') [MCtrl])            -> return $ selectNextLayer s
        (EvKey (KChar 'p') [MCtrl])            -> return $ selectPrevLayer s
        (EvKey (KChar 'u') [MCtrl])            -> return $ moveCurrentLayerUp s
        (EvKey (KChar 'd') [MCtrl])            -> return $ moveCurrentLayerDown s
        (EvKey (KChar 'v') [MCtrl])            -> return $ toggleCurrentLayer s
        (EvKey (KChar 'C') [])                 -> return $ recenterCanvas s
        (EvKey (KChar 'f') [])                 -> return $ beginFgPaletteSelect s
        (EvKey (KChar 'b') [])                 -> return $ beginBgPaletteSelect s
        (EvKey (KChar '>') [])                 -> return $ increaseToolSize s
        (EvKey (KChar '<') [])                 -> return $ decreaseToolSize s
        (EvKey KEsc []) | isJust (s^.dragging) -> return $ cancelDragging s
        (EvKey (KChar c) []) | isDigit c       -> return $ setToolByChar c s
        (EvKey (KChar 'c') [])                 -> return $ whenTool s charTools
                                                           beginCharacterSelect
        (EvKey (KChar '+') [])                 -> increaseCanvasSize s
        (EvKey (KChar '-') [])                 -> decreaseCanvasSize s
        (EvKey (KChar 'a') [MCtrl])            -> addLayer s
        (EvKey (KChar 'u') [])                 -> undo s
        (EvKey (KChar 'r') [])                 -> redo s
        _                                      -> return s
handleEvent s _ = continue s
