module Events.Main
  ( handleMainEvent
  )
where

import Brick

import Types

handleMainEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleMainEvent s _ = halt s
