module UI
  ( drawUI
  )
where

import Brick

import Types

drawUI :: AppState -> [Widget Name]
drawUI = const [str "Hello"]
