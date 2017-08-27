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

handleBoxStyleSelectEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleBoxStyleSelectEvent s e = do
    result <- handleCommonEvent s e
    case result of
        Just s' -> continue s'
        Nothing -> handleEvent s e

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (MouseDown (BoxStyleSelectorEntry i) _ _ _) = do
    continue $ popMode $ s & boxStyleIndex .~ i
handleEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | isDigit c = do
    let i = read [c]
    case i >= 0 && i < length boxStyles of
        True -> continue $ popMode $ s & boxStyleIndex .~ i
        False -> continue s
handleEvent s (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    continue s
handleEvent s _ =
    continue $ popMode s
