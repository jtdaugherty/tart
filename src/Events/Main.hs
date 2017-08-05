module Events.Main
  ( handleMainEvent
  )
where

import Brick

import Types

handleMainEvent :: AppState -> BrickEvent e n -> EventM n (Next AppState)
handleMainEvent s _ = continue s
