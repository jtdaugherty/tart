module Events
  ( handleEvent
  )
where

import Brick
import Lens.Micro.Platform
import qualified Graphics.Vty as V

import Types
import Events.Main
import Util

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvResize _ _)) =
    continue =<< resizeCanvas s
handleEvent s e =
    case s^.mode of
        Main -> handleMainEvent s e
