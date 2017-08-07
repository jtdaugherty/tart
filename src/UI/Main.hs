module UI.Main
  ( drawMainUI
  , toolSelectorEntryWidth
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import qualified Data.Array.Unboxed as A
import Lens.Micro.Platform
import Data.Word (Word64)

import Types
import UI.Common
import Theme
import Util

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
        toolbarEntries = [ padLeft (Pad 1) $ drawToolSelector s
                         , drawChar s
                         , fgPal
                         , bgPal
                         ]
    in clickable Hud $
       vBox [ hBox $ padRight (Pad 1) <$> toolbarEntries
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
    let Just idx = lookup (s^.tool) tools
    in clickable ToolSelector $
       borderWithLabel ((withDefAttr keybindingAttr $ str "T") <+> str "ool") $
       hLimit toolSelectorEntryWidth $
       hCenter $
       (withDefAttr keybindingAttr (str $ show idx)) <+>
       (str $ ":" <> show (s^.tool))

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
canvas s = clickable Canvas $ raw $ canvasToImage $ s^.drawingFrozen

canvasToImage :: A.UArray (Int, Int) Word64 -> V.Image
canvasToImage a =
    let (_, (lastCol, lastRow)) = A.bounds a
        rows = getRow <$> [0..lastRow]
        getRow r = V.horizCat $ (uncurry $ flip V.char) <$> decodePixel <$> getCol r <$> [0..lastCol]
        getCol r c = a A.! (c, r)
    in V.vertCat rows
