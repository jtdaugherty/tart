module UI.AskToSave
  ( drawAskToSaveUI
  )
where

import qualified Data.Text as T
import Lens.Micro.Platform

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit

import Types
import Theme

drawAskToSaveUI :: AppState -> [Widget Name]
drawAskToSaveUI s = [drawPromptWindow s]

drawPromptWindow :: AppState -> Widget Name
drawPromptWindow s =
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
        body = (hCenter $ str "You have unsaved changes. Save them?") <=>
               (hCenter help) <=>
               padTopBottom 1 fn
        renderString = txt . T.unlines
        fn = str "Path: " <+> renderEditor renderString True (s^.askToSaveFilenameEdit)
