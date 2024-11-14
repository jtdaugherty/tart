module Events.BoxStyleSelect
  ( handleBoxStyleSelectEvent
  )
where

import Brick
import Data.Char (isDigit)
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Types
import State
import Events.Common

handleBoxStyleSelectEvent :: BrickEvent Name e -> EventM Name AppState ()
handleBoxStyleSelectEvent e = do
    result <- handleCommonEvent e
    case result of
        True -> return ()
        False -> handleEvent e

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (MouseDown (BoxStyleSelectorEntry i) _ _ _) = do
    boxStyleIndex .= i
    modify popMode
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c = do
    let i = read [c]
    case i >= 0 && i < length boxStyles of
        True -> do
            modify popMode
            boxStyleIndex .= i
        False -> return ()
handleEvent (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    return ()
handleEvent _ =
    modify popMode
