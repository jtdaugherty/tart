module Events.PaletteEntrySelect
  ( handlePaletteEntrySelectEvent
  )
where

import Brick
import Lens.Micro.Platform

import Types
import Util

handlePaletteEntrySelectEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handlePaletteEntrySelectEvent s (MouseDown FgSelector _ _ _) = do
    continue $ beginFgPaletteSelect s
handlePaletteEntrySelectEvent s (MouseDown BgSelector _ _ _) = do
    continue $ beginBgPaletteSelect s
handlePaletteEntrySelectEvent s (MouseDown (FgPaletteEntry idx) _ _ _) = do
    continue $ setFgPaletteIndex s idx
handlePaletteEntrySelectEvent s (MouseDown (BgPaletteEntry idx) _ _ _) = do
    continue $ setBgPaletteIndex s idx
handlePaletteEntrySelectEvent s (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    continue s
handlePaletteEntrySelectEvent s _ =
    continue $ s & mode .~ Main
