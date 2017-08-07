module Events.PaletteEntrySelect
  ( handlePaletteEntrySelectEvent
  )
where

import Brick
import Data.Char (isDigit)
import Lens.Micro.Platform
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

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
handleEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c = do
    let idx = read [c]
    case idx >= 0 && idx < Vec.length (s^.palette) of
        False -> continue s
        True -> case s^.mode of
            FgPaletteEntrySelect -> continue $ setFgPaletteIndex s idx
            BgPaletteEntrySelect -> continue $ setBgPaletteIndex s idx
            _ -> continue s

handleEvent s (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    continue s
handleEvent s _ =
    continue $ s & mode .~ Main
