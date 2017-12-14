{-# LANGUAGE OverloadedStrings #-}
module Theme
  ( theme

  , keybindingAttr
  , selectedLayerAttr
  , clickableAttr
  , errorAttr
  )
where

import Brick
import Brick.Widgets.Edit
import Graphics.Vty

keybindingAttr :: AttrName
keybindingAttr = "keybinding"

selectedLayerAttr :: AttrName
selectedLayerAttr = "selectedLayer"

clickableAttr :: AttrName
clickableAttr = "clickable"

errorAttr :: AttrName
errorAttr = "error"

theme :: AttrMap
theme = attrMap defAttr
  [ (keybindingAttr,        fg white `withStyle` underline)
  , (editAttr,              black `on` yellow)
  , (editFocusedAttr,       black `on` yellow)
  , (selectedLayerAttr,     white `on` magenta)
  , (clickableAttr,         fg white `withStyle` bold)
  , (errorAttr,             fg red)
  ]
