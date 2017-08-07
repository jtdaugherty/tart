{-# LANGUAGE OverloadedStrings #-}
module Theme
  ( theme

  , keybindingAttr
  )
where

import Brick
import Graphics.Vty

keybindingAttr :: AttrName
keybindingAttr = "keybinding"

theme :: AttrMap
theme = attrMap defAttr
  [ (keybindingAttr, fg white)
  ]
