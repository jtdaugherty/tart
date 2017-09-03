module UI.AskToSave
  ( drawAskToSaveUI
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Types
import Theme

drawAskToSaveUI :: AppState -> [Widget Name]
drawAskToSaveUI _ = [drawPromptWindow]

drawPromptWindow :: Widget Name
drawPromptWindow =
    centerLayer $
    borderWithLabel (str "Save") $
        hLimit 60 $
        padLeftRight 2 $ padTopBottom 1 body
    where
        help = hBox [ str "("
                    , withDefAttr keybindingAttr $ str "Esc"
                    , str " to quit without saving, "
                    , withDefAttr keybindingAttr $ str "Enter"
                    , str " to save and quit)"
                    ]
        body = (hCenter $ str "You have unsaved changes. Save them? (y/n)") <=>
               (hCenter help)
