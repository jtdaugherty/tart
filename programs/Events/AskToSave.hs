module Events.AskToSave
  ( handleAskToSaveEvent
  )
where

import qualified Graphics.Vty as V

import Brick

import Types
import State

handleAskToSaveEvent :: BrickEvent Name e -> EventM Name AppState ()
handleAskToSaveEvent (VtyEvent (V.EvKey V.KEsc [])) =
    halt
handleAskToSaveEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) =
    halt
handleAskToSaveEvent (VtyEvent (V.EvKey (V.KChar 'y') [])) =
    askForSaveFilename True
handleAskToSaveEvent _ =
    return ()
