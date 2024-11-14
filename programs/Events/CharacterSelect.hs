module Events.CharacterSelect
  ( handleCharacterSelectEvent
  )
where

import Brick
import qualified Graphics.Vty as V

import Types
import State

handleCharacterSelectEvent :: BrickEvent Name e -> EventM Name AppState ()
handleCharacterSelectEvent (VtyEvent (V.EvKey V.KEsc _)) =
    cancelCharacterSelect
handleCharacterSelectEvent (VtyEvent (V.EvKey (V.KChar c) [])) =
    selectCharacter c
handleCharacterSelectEvent _ =
    return ()
