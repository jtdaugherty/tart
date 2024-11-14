{-# LANGUAGE OverloadedStrings #-}
module Theme
  ( theme

  , keybindingAttr
  , selectedLayerAttr
  , clickableAttr
  , errorAttr
  , headerAttr
  )
where

import Brick
import Brick.Widgets.Edit
import Graphics.Vty

keybindingAttr :: AttrName
keybindingAttr = attrName "keybinding"

selectedLayerAttr :: AttrName
selectedLayerAttr = attrName "selectedLayer"

clickableAttr :: AttrName
clickableAttr = attrName "clickable"

headerAttr :: AttrName
headerAttr = attrName "header"

errorAttr :: AttrName
errorAttr = attrName "error"

theme :: AttrMap
theme = attrMap defAttr
  [ (keybindingAttr,        fg white `withStyle` underline)
  , (editAttr,              black `on` yellow)
  , (editFocusedAttr,       black `on` yellow)
  , (selectedLayerAttr,     white `on` magenta)
  , (clickableAttr,         fg white `withStyle` bold)
  , (headerAttr,            fg white `withStyle` underline)
  , (errorAttr,             fg red)
  ]
