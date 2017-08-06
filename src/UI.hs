module UI
  ( drawUI
  )
where

import Brick
import Lens.Micro.Platform

import Types
import UI.Main
import UI.CharacterSelect

drawUI :: AppState -> [Widget Name]
drawUI s =
    case s^.mode of
        Main -> drawMainUI s
        CharacterSelect -> drawCharacterSelectUI s
