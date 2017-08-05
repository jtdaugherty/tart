module Events
  ( handleEvent
  )
where

import Brick

import Types

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s _ = halt s
