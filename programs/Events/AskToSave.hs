module Events.AskToSave
  ( handleAskToSaveEvent
  )
where

import qualified Graphics.Vty as V

import Brick

import Types
import State

handleAskToSaveEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleAskToSaveEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    halt s
handleAskToSaveEvent s (VtyEvent (V.EvKey (V.KChar 'n') [])) =
    halt s
handleAskToSaveEvent s (VtyEvent (V.EvKey (V.KChar 'y') [])) =
    continue $ askForSaveFilename True s
handleAskToSaveEvent s _ =
    continue s
