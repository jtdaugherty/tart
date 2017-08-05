module Events.Main
  ( handleMainEvent
  )
where

import Brick
import qualified Graphics.Vty as V

import Types
import Draw

handleMainEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleMainEvent s (MouseDown Canvas _ _ (Location l)) = do
    continue =<< drawAtPoint s l
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleMainEvent s _ = continue s
