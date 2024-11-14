module Events.ToolSelect
  ( handleToolSelectEvent
  )
where

import Brick
import Data.Char (isDigit)
import qualified Graphics.Vty as V

import Types
import State
import Events.Common

handleToolSelectEvent :: BrickEvent Name e -> EventM Name AppState ()
handleToolSelectEvent e = do
    result <- handleCommonEvent e
    case result of
        True -> return ()
        False -> handleEvent e

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (MouseDown (ToolSelectorEntry t) _ _ _) = do
    setTool t
    modify popMode
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c = do
    let idx = read [c]
    case filter ((== idx) . snd) tools of
        [(t, _)] -> do
            setTool t
            modify popMode
        _ -> return ()
handleEvent (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    return ()
handleEvent _ =
    modify popMode
