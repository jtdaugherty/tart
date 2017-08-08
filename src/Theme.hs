{-# LANGUAGE OverloadedStrings #-}
module Theme
  ( theme

  , keybindingAttr
  )
where

import Brick
import Brick.Widgets.Edit
import Graphics.Vty

keybindingAttr :: AttrName
keybindingAttr = "keybinding"

theme :: AttrMap
theme = attrMap defAttr
  [ (keybindingAttr, fg white)
  , (editAttr, black `on` yellow)
  , (editFocusedAttr, black `on` yellow)
  ]
