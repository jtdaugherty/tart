module UI.Main
  ( drawMainUI
  )
where

import Brick
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Data.Vector as Vec

import Types

drawMainUI :: AppState -> [Widget Name]
drawMainUI s =
    [ maybeHud s
    , canvas s
    ]

maybeHud :: AppState -> Widget Name
maybeHud s =
    case s^.showHud of
        False -> emptyWidget
        True -> hud s

hud :: AppState -> Widget Name
hud s = str $ "[tool:" <> show (s^.tool) <> "]"

canvas :: AppState -> Widget Name
canvas s = clickable Canvas $ raw $ canvasToImage $ s^.drawing

canvasToImage :: Vec.Vector (Vec.Vector Char) -> V.Image
canvasToImage a =
    let getRow r = V.string V.defAttr $ Vec.toList r
        rows = Vec.toList $ getRow <$> a
    in V.vertCat rows
