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

handleCanvasSizePromptEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleCanvasSizePromptEvent s (MouseDown CanvasSizeWidthEdit _ _ _) =
    case focusGetCurrent (s^.canvasSizeFocus) of
        Just CanvasSizeWidthEdit -> continue s
        _ -> continue $ s & canvasSizeFocus %~ focusNext
handleCanvasSizePromptEvent s (MouseDown CanvasSizeHeightEdit _ _ _) =
    case focusGetCurrent (s^.canvasSizeFocus) of
        Just CanvasSizeHeightEdit -> continue s
        _ -> continue $ s & canvasSizeFocus %~ focusNext
handleCanvasSizePromptEvent s (MouseDown _ _ _ _) =
    continue $ popMode s
handleCanvasSizePromptEvent s (VtyEvent (V.EvKey (V.KChar '\t') [])) =
    continue $ s & canvasSizeFocus %~ focusNext
handleCanvasSizePromptEvent s (VtyEvent (V.EvKey V.KBackTab [])) =
    continue $ s & canvasSizeFocus %~ focusPrev
handleCanvasSizePromptEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    continue $ popMode s
handleCanvasSizePromptEvent s (VtyEvent (V.EvKey V.KEnter [])) =
    continue =<< tryResizeCanvas s
handleCanvasSizePromptEvent s (VtyEvent e) =
    case focusGetCurrent (s^.canvasSizeFocus) of
        Just CanvasSizeWidthEdit ->
            continue =<< handleEventLensed s canvasSizeWidthEdit handleEditorEvent e
        Just CanvasSizeHeightEdit ->
            continue =<< handleEventLensed s canvasSizeHeightEdit handleEditorEvent e
        _ -> continue s
handleCanvasSizePromptEvent s _ =
    continue s
