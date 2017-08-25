{-# LANGUAGE OverloadedStrings #-}
module Theme
  ( theme

  , keybindingAttr
  , selectedLayerAttr
  )
where

import Brick
import Brick.Widgets.Edit
import Graphics.Vty

keybindingAttr :: AttrName
keybindingAttr = "keybinding"

selectedLayerAttr :: AttrName
selectedLayerAttr = "selectedLayer"

theme :: AttrMap
theme = attrMap defAttr
  [ (keybindingAttr, fg white `withStyle` underline)
  , (editAttr, black `on` yellow)
  , (editFocusedAttr, black `on` yellow)
  , (selectedLayerAttr, black `on` magenta)
  ]
