module UI.Main
  ( drawMainUI
  )
where

import Brick

import Types

drawMainUI :: AppState -> [Widget Name]
drawMainUI = const [str "main"]
