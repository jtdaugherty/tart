module Events.ToolSelect
  ( handleToolSelectEvent
  )
where

import Brick
import Data.Char (isDigit)
import qualified Graphics.Vty as V

import Types
import Util
import Events.Common

handleToolSelectEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleToolSelectEvent s e = do
    result <- handleCommonEvent s e
    case result of
        Just s' -> continue s'
        Nothing -> handleEvent s e

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (MouseDown (ToolSelectorEntry t) _ _ _) = do
    continue $ popMode $ setTool s t
handleEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c = do
    let idx = read [c]
    case filter ((== idx) . snd) tools of
        [(t, _)] -> continue $ popMode $ setTool s t
        _ -> continue s
handleEvent s (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    continue s
handleEvent s _ =
    continue $ popMode s
