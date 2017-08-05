module Events
  ( handleEvent
  )
where

import Brick
import Lens.Micro.Platform

import Types
import Events.Main

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s e =
    case s^.mode of
        Main -> handleMainEvent s e
