module Events.StyleSelect
  ( handleStyleSelectEvent
  )
where

import Brick
import Lens.Micro.Platform

import Types
import State
import Events.Common

handleStyleSelectEvent :: BrickEvent Name e -> EventM Name AppState ()
handleStyleSelectEvent e = do
    result <- handleCommonEvent e
    case result of
        True -> return ()
        False -> handleEvent e

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent e) | isStyleKey e = do
    toggleStyleFromKey e
    modify popMode
handleEvent (MouseDown (StyleSelectorEntry sty) _ _ _) = do
    drawStyle %= toggleStyle sty
    modify popMode
handleEvent (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    return ()
handleEvent _ =
    modify popMode
