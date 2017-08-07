module Events.PaletteEntrySelect
  ( handlePaletteEntrySelectEvent
  )
where

import Brick
import Lens.Micro.Platform

import Types
import Util

import Events.Common

handlePaletteEntrySelectEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handlePaletteEntrySelectEvent s e = do
    result <- handleCommonEvent s e
    case result of
        Just s' -> continue s'
        Nothing -> handleEvent s e

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (MouseDown FgSelector _ _ _) = do
    continue $ beginFgPaletteSelect s
handleEvent s (MouseDown BgSelector _ _ _) = do
    continue $ beginBgPaletteSelect s
handleEvent s (MouseDown (FgPaletteEntry idx) _ _ _) = do
    continue $ setFgPaletteIndex s idx
handleEvent s (MouseDown (BgPaletteEntry idx) _ _ _) = do
    continue $ setBgPaletteIndex s idx
handleEvent s (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    continue s
handleEvent s _ =
    continue $ s & mode .~ Main
