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
hud s =
    let fgPal = drawPalette (s^.palette) "fg" (s^.drawFgPaletteIndex) FgPaletteEntry
        bgPal = drawPalette (s^.palette) "bg" (s^.drawBgPaletteIndex) BgPaletteEntry
    in clickable Hud $
       vBox [ drawTool s <+> str " " <+> drawChar s <+> str " " <+> fgPal <+> str " " <+> bgPal
            , hBorderWithLabel (str "Press 'h' to hide")
            ]

drawChar :: AppState -> Widget Name
drawChar s = padTop (Pad 1) $ str $ "char:[" <> [s^.drawCharacter] <> "]"

drawTool :: AppState -> Widget Name
drawTool s = padTop (Pad 1) $ str $ "tool:" <> show (s^.tool)

drawPalette :: Vec.Vector V.Color -> String -> Int -> (Int -> Name) -> Widget Name
drawPalette pal label curIdx mkName =
    hBox (border curColor : str " " : entries)
    where
        curColor = hBox [ str $ label <> ":"
                        , drawPaletteEntry ( curIdx
                                           , Vec.unsafeIndex pal curIdx
                                           )
                        , str " "
                        ]
        entries = padTopBottom 1 <$> drawPaletteEntry <$> (zip [0..] $ Vec.toList pal)
        drawPaletteEntry (idx, color) =
            clickable (mkName idx) $
            raw $ V.string (V.defAttr `V.withBackColor` color) "  "

canvas :: AppState -> Widget Name
canvas s = clickable Canvas $ raw $ canvasToImage $ s^.drawing

canvasToImage :: Vec.Vector (Vec.Vector Pixel) -> V.Image
canvasToImage a =
    let getRow r = V.horizCat $ (uncurry $ flip V.char) <$> Vec.toList r
        rows = Vec.toList $ getRow <$> a
    in V.vertCat rows
