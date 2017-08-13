module Events.CharacterSelect
  ( handleCharacterSelectEvent
  )
where

import Brick
import qualified Graphics.Vty as V

import Types
import Util

handleCharacterSelectEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleCharacterSelectEvent s (VtyEvent (V.EvKey V.KEsc _)) =
    continue $ cancelCharacterSelect s
handleCharacterSelectEvent s (VtyEvent (V.EvKey (V.KChar c) [])) =
    continue $ selectCharacter c s
handleCharacterSelectEvent s _ =
    continue s
