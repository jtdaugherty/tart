module Events.ToolSelect
  ( handleToolSelectEvent
  )
where

import Brick
import Lens.Micro.Platform

import Types
import Util

handleToolSelectEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleToolSelectEvent s (MouseDown (ToolSelectorEntry t) _ _ _) = do
    continue $ (setTool s t) & mode .~ Main
handleToolSelectEvent s (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    continue s
handleToolSelectEvent s _ =
    continue $ s & mode .~ Main
