module UI.Main
  ( drawMainUI
  )
where

import Brick
import Brick.Widgets.Border
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Data.Vector as Vec

import Types
import UI.Common

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
hud s =
    let fgPal = drawPaletteSelector (s^.palette) "FG" (s^.drawFgPaletteIndex) FgSelector
        bgPal = drawPaletteSelector (s^.palette) "BG" (s^.drawBgPaletteIndex) BgSelector
    in clickable Hud $
       vBox [ drawToolSelector s <+> str " " <+> drawChar s <+> str " " <+> fgPal <+> str " " <+> bgPal
            , hBorderWithLabel (str "Press 'h' to hide")
            ]

drawChar :: AppState -> Widget Name
drawChar s = borderWithLabel (str "Char") $ padLeftRight 2 $ str [s^.drawCharacter]

drawToolSelector :: AppState -> Widget Name
drawToolSelector s =
    clickable ToolSelector $
    borderWithLabel (str "Tool") $
    str $ show (s^.tool)

drawPaletteSelector :: Vec.Vector V.Color -> String -> Int -> Name -> Widget Name
drawPaletteSelector pal label curIdx selName =
    (clickable selName $ borderWithLabel (str label) curColor)
    where
        curColor = drawPaletteEntry pal curIdx 2

canvas :: AppState -> Widget Name
canvas s = clickable Canvas $ raw $ canvasToImage $ s^.drawing

canvasToImage :: Vec.Vector (Vec.Vector Pixel) -> V.Image
canvasToImage a =
    let getRow r = V.horizCat $ (uncurry $ flip V.char) <$> Vec.toList r
        rows = Vec.toList $ getRow <$> a
    in V.vertCat rows
