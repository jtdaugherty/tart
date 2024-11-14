module Events.PaletteEntrySelect
  ( handlePaletteEntrySelectEvent
  )
where

import Brick

import Types
import State

import Events.Common

handlePaletteEntrySelectEvent :: BrickEvent Name e -> EventM Name AppState ()
handlePaletteEntrySelectEvent e = do
    result <- handleCommonEvent e
    case result of
        True -> return ()
        False -> handleEvent e

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (MouseDown FgSelector _ _ _) = do
    beginFgPaletteSelect
handleEvent (MouseDown BgSelector _ _ _) = do
    beginBgPaletteSelect
handleEvent (MouseDown (FgPaletteEntry idx) _ _ _) = do
    setFgPaletteIndex idx
handleEvent (MouseDown (BgPaletteEntry idx) _ _ _) = do
    setBgPaletteIndex idx
handleEvent (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    return ()
handleEvent _ =
    modify popMode
