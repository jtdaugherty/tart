module UI.Main
  ( drawMainUI
  )
where

import Brick
import Brick.Widgets.Border
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
hud s = (drawTool s <+> str " " <+> drawPalette s) <=> hBorder

drawTool :: AppState -> Widget Name
drawTool s = str $ "tool:" <> show (s^.tool)

drawPalette :: AppState -> Widget Name
drawPalette s =
    hBox (curColor : str "palette:" : entries)
    where
        curColor = hBox [ str "current:"
                        , drawPaletteEntry ( s^.drawPaletteIndex
                                           , Vec.unsafeIndex (s^.palette) (s^.drawPaletteIndex)
                                           )
                        , str " "
                        ]
        entries = drawPaletteEntry <$> (zip [0..] $ Vec.toList $ s^.palette)
        drawPaletteEntry (idx, color) =
            clickable (PaletteEntry idx) $
            raw $ V.string (V.defAttr `V.withBackColor` color) "  "

canvas :: AppState -> Widget Name
canvas s = clickable Canvas $ raw $ canvasToImage $ s^.drawing

canvasToImage :: Vec.Vector (Vec.Vector Pixel) -> V.Image
canvasToImage a =
    let getRow r = V.horizCat $ (uncurry $ flip V.char) <$> Vec.toList r
        rows = Vec.toList $ getRow <$> a
    in V.vertCat rows
