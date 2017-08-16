module Events.StyleSelect
  ( handleStyleSelectEvent
  )
where

import Brick
import Lens.Micro.Platform

import Types
import Util
import Events.Common

handleStyleSelectEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleStyleSelectEvent s e = do
    result <- handleCommonEvent s e
    case result of
        Just s' -> continue s'
        Nothing -> handleEvent s e

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent e) | isStyleKey e =
    continue $ setMode Main $ toggleStyleFromKey e s
handleEvent s (MouseDown (StyleSelectorEntry sty) _ _ _) = do
    continue $ setMode Main $ s & drawStyle %~ toggleStyle sty
handleEvent s (MouseUp _ _ _) =
    -- Ignore mouse-up events so we don't go back to Main mode. This
    -- includes mouse-up events generated in this mode, in addition to
    -- the mouse-up event generated just after we switch into this mode
    -- from Main.
    continue s
handleEvent s _ =
    continue $ setMode Main s
