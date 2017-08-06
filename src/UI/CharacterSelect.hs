module UI.CharacterSelect
  ( drawCharacterSelectUI
  )
where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border

import UI.Main
import Types

drawCharacterSelectUI :: AppState -> [Widget Name]
drawCharacterSelectUI s = prompt : drawMainUI s

prompt :: Widget Name
prompt =
    centerLayer $
    borderWithLabel (str "Choose a Character") $
    padTopBottom 1 $
    padLeftRight 2 $
    str "Press a character to draw with."
