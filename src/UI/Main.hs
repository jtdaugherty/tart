module UI.Main
  ( drawMainUI
  , toolSelectorEntryWidth
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Types
import UI.Common
import Theme
import Util
import Canvas
import Draw (boxCorners)

drawMainUI :: AppState -> [Widget Name]
drawMainUI s =
    concat $ catMaybes [ Just [maybeHud s]
                       , maybeDrawBox s
                       , Just [canvas s]
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
                         , drawCanvasSize s
                         ]
        filename = case s^.canvasPath of
            Nothing -> "<unsaved>"
            Just p -> p
        modified = if not $ s^.canvasDirty then "" else "*"
    in clickable Hud $
       vBox [ hCenter $ padLeft (Pad 1) $ hBox $ padRight (Pad 1) <$> toolbarEntries
            , hBox [borderElem bsHorizontal <+> str ("[" <> filename <> modified <> "]") <+> hBorder]
            ]

drawCanvasSize :: AppState -> Widget Name
drawCanvasSize s =
    let (width, height) = canvasSize $ s^.drawing
    in clickable ResizeCanvas $
       borderWithLabel (str "Can" <+> (withDefAttr keybindingAttr (str "v")) <+> str "as") $
       str $ show width <> " columns, " <> show height <> " rows"

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
        curColor = drawPaletteEntry s curIdx 4 isFg

canvas :: AppState -> Widget Name
canvas s =
    centerAbout (s^.canvasOffset) $
    border $
    clickable Canvas $
    raw $ canvasToImage (s^.drawing)

canvasToImage :: Canvas -> V.Image
canvasToImage a =
    let (lastCol, lastRow) = canvasSize a & each %~ pred
        rows = getRow <$> [0..lastRow]
        getRow r = V.horizCat $ (uncurry $ flip V.char) <$> getCol r <$> [0..lastCol]
        getCol r c = canvasGetPixel a (c, r)
    in V.vertCat rows

maybeDrawBox :: AppState -> Maybe [Widget Name]
maybeDrawBox s = do
    (n, l0, l1) <- s^.dragging
    if n == Canvas && s^.tool == Box
       then Just $ drawBox s l0 l1
       else Nothing

drawBox :: AppState -> Location -> Location -> [Widget Name]
drawBox s a b =
    -- Generate the list of columns and rows that make up the box
    let (ul, lr) = boxCorners a b
        height = lr^._2 - ul^._2 + 1
        width = lr^._1 - ul^._1 + 1
        upperLeft = (ul^._1 + can0, ul^._2 + can1)
        horiz = replicate (width - 2) '-'
        corner = "+"
        topBottom = V.string (currentPaletteAttribute s) $
                      if width <= 1
                         then corner
                         else corner <> horiz <> corner
        leftRight = V.charFill (currentPaletteAttribute s) '|' 1 (height - 2)
        Just cExtent = s^.canvasExtent
        Location (can0, can1) = extentUpperLeft cExtent
    in catMaybes [ Just $ translateBy (Location upperLeft) $ raw topBottom
                 , if height > 1
                      then Just $ translateBy (Location $ upperLeft & _2 %~ (+ (height - 1))) $ raw topBottom
                      else Nothing
                 , Just $ translateBy (Location $ upperLeft & _2 %~ succ) $ raw leftRight
                 , if width > 1
                      then Just $ translateBy (Location $ upperLeft & _2 %~ succ & _1 %~ (+ (width - 1))) $ raw leftRight
                      else Nothing
                 ]
