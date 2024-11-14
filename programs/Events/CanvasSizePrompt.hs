module Events.CanvasSizePrompt
  ( handleCanvasSizePromptEvent
  )
where

import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Brick
import Brick.Focus
import Brick.Widgets.Edit

import Types
import State

handleCanvasSizePromptEvent :: BrickEvent Name e -> EventM Name AppState ()
handleCanvasSizePromptEvent (MouseDown CanvasSizeWidthEdit _ _ _) =
    canvasSizeFocus %= focusSetCurrent CanvasSizeWidthEdit
handleCanvasSizePromptEvent (MouseDown CanvasSizeHeightEdit _ _ _) =
    canvasSizeFocus %= focusSetCurrent CanvasSizeHeightEdit
handleCanvasSizePromptEvent (MouseDown _ _ _ _) =
    modify popMode
handleCanvasSizePromptEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) =
    canvasSizeFocus %= focusNext
handleCanvasSizePromptEvent (VtyEvent (V.EvKey V.KBackTab [])) =
    canvasSizeFocus %= focusPrev
handleCanvasSizePromptEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify popMode
handleCanvasSizePromptEvent (VtyEvent (V.EvKey V.KEnter [])) =
    tryResizeCanvas
handleCanvasSizePromptEvent e = do
    foc <- use canvasSizeFocus
    case focusGetCurrent foc of
        Just CanvasSizeWidthEdit ->
            zoom canvasSizeWidthEdit $ handleEditorEvent e
        Just CanvasSizeHeightEdit ->
            zoom canvasSizeHeightEdit $ handleEditorEvent e
        _ -> return ()
