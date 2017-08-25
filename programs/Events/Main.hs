module Events.Main
  ( handleMainEvent
  , handleAttrEvent
  )
where

import Brick
import Data.Char (isDigit)
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Data.Text.Encoding (decodeUtf8)

import Types
import Draw
import Util
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
handleAttrEvent s (MouseDown FgSelector _ _ _) = do
    return $ Just $ beginFgPaletteSelect s
handleAttrEvent s (MouseDown BgSelector _ _ _) = do
    return $ Just $ beginBgPaletteSelect s
handleAttrEvent s (MouseDown StyleSelector _ _ _) = do
    return $ Just $ beginStyleSelect s
handleAttrEvent _ _ = return Nothing

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent e) | isStyleKey e =
    continue $ toggleStyleFromKey e s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'w') [])) = do
    continue $ canvasMoveDown s
handleEvent s (VtyEvent (V.EvKey (V.KChar 's') [])) = do
    continue $ canvasMoveUp s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
    continue $ canvasMoveLeft s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
    continue $ canvasMoveRight s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'v') [])) = do
    continue $ beginCanvasSizePrompt s
handleEvent s (MouseDown ResizeCanvas _ _ _) = do
    continue $ beginCanvasSizePrompt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'C') [])) = do
    continue $ recenterCanvas s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
    continue $ beginFgPaletteSelect s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) = do
    continue $ beginBgPaletteSelect s
handleEvent s (MouseDown ToolSelector _ _ _) = do
    continue $ beginToolSelect s
handleEvent s (MouseDown IncreaseEraserSize _ _ _) = do
    continue $ increaseEraserSize s
handleEvent s (MouseDown DecreaseEraserSize _ _ _) = do
    continue $ decreaseEraserSize s
handleEvent s (MouseDown IncreaseRepaintSize _ _ _) = do
    continue $ increaseRepaintSize s
handleEvent s (MouseDown DecreaseRepaintSize _ _ _) = do
    continue $ decreaseRepaintSize s
handleEvent s (MouseDown IncreaseRestyleSize _ _ _) = do
    continue $ increaseRestyleSize s
handleEvent s (MouseDown DecreaseRestyleSize _ _ _) = do
    continue $ decreaseRestyleSize s
handleEvent s (MouseDown _ V.BScrollUp _ _) = do
    let f = case s^.tool of
              Repaint -> increaseRepaintSize
              Restyle -> increaseRestyleSize
              Eraser -> increaseEraserSize
              _ -> id
    continue $ f s
handleEvent s (MouseDown _ V.BScrollDown _ _) = do
    let f = case s^.tool of
              Repaint -> decreaseRepaintSize
              Restyle -> decreaseRestyleSize
              Eraser -> decreaseEraserSize
              _ -> id
    continue $ f s
handleEvent s (VtyEvent (V.EvKey (V.KChar '>') [])) = do
    continue $ case s^.tool of
        Eraser -> increaseEraserSize s
        Repaint -> increaseRepaintSize s
        Restyle -> increaseRestyleSize s
        _ -> s
handleEvent s (VtyEvent (V.EvKey (V.KChar '<') [])) = do
    continue $ case s^.tool of
        Eraser -> decreaseEraserSize s
        Repaint -> decreaseRepaintSize s
        Restyle -> decreaseRestyleSize s
        _ -> s
handleEvent s (MouseDown BoxStyleSelector _ _ _) = do
    continue $ beginBoxStyleSelect s
handleEvent s (MouseDown Canvas _ _ (Location l)) = do
    continue =<< drawWithCurrentTool l s
handleEvent s (VtyEvent (V.EvKey (V.KChar '+') [])) = do
    continue =<< increaseCanvasSize s
handleEvent s (VtyEvent (V.EvKey (V.KChar '-') [])) = do
    continue =<< decreaseCanvasSize s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = do
    continue $ s & dragging .~ Nothing
handleEvent s (MouseDown (SelectLayer idx) _ _ _) = do
    continue $ s & selectedLayerIndex .~ idx
handleEvent s (VtyEvent (V.EvPaste bytes)) = do
    continue =<< pasteTextAtPoint (0, 0) s (decodeUtf8 bytes)
handleEvent s (AppEvent (DragFinished n _ _)) = do
    continue =<< handleDragFinished s n
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
    quit True s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'u') [])) = do
    continue =<< undo s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'r') [])) = do
    continue =<< redo s
handleEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c = do
    let idx = read [c]
    case filter ((== idx) . snd) tools of
        [(t, _)] -> continue $ popMode $ setTool s t
        _ -> continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) =
    continue $ whenTool s [Freehand, FloodFill] beginCharacterSelect
handleEvent s (MouseDown CharSelector _ _ _) =
    continue $ whenTool s [Freehand, FloodFill] beginCharacterSelect
handleEvent s _ = continue s

whenTool :: AppState -> [Tool] -> (AppState -> AppState) -> AppState
whenTool s ts f = if s^.tool `elem` ts then f s else s
