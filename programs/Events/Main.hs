module Events.Main
  ( handleMainEvent
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
        Nothing -> handleEvent s e

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
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
handleEvent s (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
    continue $ beginFgPaletteSelect s
handleEvent s (MouseDown FgSelector _ _ _) = do
    continue $ beginFgPaletteSelect s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) = do
    continue $ beginBgPaletteSelect s
handleEvent s (MouseDown BgSelector _ _ _) = do
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
handleEvent s (VtyEvent (V.EvKey (V.KChar '>') [])) = do
    continue $ case s^.tool of
        Eraser -> increaseEraserSize s
        Repaint -> increaseRepaintSize s
        _ -> s
handleEvent s (VtyEvent (V.EvKey (V.KChar '<') [])) = do
    continue $ case s^.tool of
        Eraser -> decreaseEraserSize s
        Repaint -> decreaseRepaintSize s
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
        [(t, _)] -> continue $ setMode Main $ setTool s t
        _ -> continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) =
    continue $ whenTool s [Freehand, FloodFill] beginCharacterSelect
handleEvent s (MouseDown CharSelector _ _ _) =
    continue $ whenTool s [Freehand, FloodFill] beginCharacterSelect
handleEvent s _ = continue s

whenTool :: AppState -> [Tool] -> (AppState -> AppState) -> AppState
whenTool s ts f = if s^.tool `elem` ts then f s else s
