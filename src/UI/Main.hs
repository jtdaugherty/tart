module UI.Main
  ( drawMainUI
  , toolSelectorEntryWidth
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Data.Vector as Vec

import Types
import UI.Common
import Theme

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
    let fgPal = drawPaletteSelector s True
        bgPal = drawPaletteSelector s False
    in clickable Hud $
       vBox [ drawToolSelector s <+> str " " <+> drawChar s <+> str " " <+> fgPal <+> str " " <+> bgPal
            , hBorderWithLabel (str "Press 'h' to hide")
            ]

drawChar :: AppState -> Widget Name
drawChar s =
    clickable CharSelector $
    borderWithLabel ((withDefAttr keybindingAttr $ str "C") <+> str "har") $
    padLeftRight 2 $ str [s^.drawCharacter]

toolSelectorEntryWidth :: Int
toolSelectorEntryWidth = 20

drawToolSelector :: AppState -> Widget Name
drawToolSelector s =
    clickable ToolSelector $
    borderWithLabel ((withDefAttr keybindingAttr $ str "T") <+> str "ool") $
    hLimit toolSelectorEntryWidth $
    hCenter $
    str $ show (s^.tool)

drawPaletteSelector :: AppState -> Bool -> Widget Name
drawPaletteSelector s isFg =
    (clickable selName $ borderWithLabel label curColor)
    where
        label = if isFg
                then (withDefAttr keybindingAttr $ str "F") <+> str "G"
                else (withDefAttr keybindingAttr $ str "B") <+> str "G"
        curIdx = if isFg then s^.drawFgPaletteIndex
                         else s^.drawBgPaletteIndex
        selName = if isFg then FgSelector else BgSelector
        curColor = drawPaletteEntry s curIdx 2 isFg

canvas :: AppState -> Widget Name
canvas s = clickable Canvas $ raw $ canvasToImage $ s^.drawing

canvasToImage :: Vec.Vector (Vec.Vector Pixel) -> V.Image
canvasToImage a =
    let getRow r = V.horizCat $ (uncurry $ flip V.char) <$> Vec.toList r
        rows = Vec.toList $ getRow <$> a
    in V.vertCat rows
