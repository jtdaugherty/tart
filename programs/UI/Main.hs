module UI.Main
  ( drawMainUI
  , toolSelectorEntryWidth
  , boxStyleSelectorEntryWidth
  , styleSelectorEntryWidth
  )
where

import Brick
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Maybe (isJust, catMaybes, fromMaybe)
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Types
import UI.Common
import Theme
import State
import Tart.Canvas

drawMainUI :: AppState -> [Widget Name]
drawMainUI s =
    catMaybes [ Just $ topHud s
              , if s^.layerListVisible then Just $ layerHud s else Nothing
              , Just $ canvas s
              ]

topHud :: AppState -> Widget Name
topHud s =
    let fgPal = drawPaletteSelector s True
        bgPal = drawPaletteSelector s False
        stySel = drawStyleSelector s
        toolbarEntries = [ drawToolSelector s
                         , toolHud s
                         , vLimit 1 $ fill ' '
                         , fgPal
                         , bgPal
                         , stySel
                         , drawCanvasSize s
                         ]
        filename = case s^.canvasPath of
            Nothing -> "<unsaved>"
            Just p -> p
        modified = if not $ s^.canvasDirty then "" else "*"
    in clickable TopHud $
       vBox [ (padLeft (Pad 1) $ hBox $ padRight (Pad 1) <$> toolbarEntries)
            , hBox [hBorder, str ("[" <> filename <> modified <> "]"), borderElem bsHorizontal]
            ]

layerHud :: AppState -> Widget Name
layerHud s = translateBy (Location (0, 4)) $
             padBottom (Pad 4) $
             (hLimit 20 $ layerList <=> fill ' ' <=> layerOptions) <+> vBorder
    where
        layerList = vBox $ (hCenter $ str "Layers") :
                           (mkEntry <$> entries) <>
                           [addLayerEntry]
        entries = [ ( i
                    , s^.layerInfoFor(i)
                    )
                  | i <- s^.layerOrder
                  ]
        addLayerEntry =
            clickable AddLayer $ hCenter $
              withDefAttr clickableAttr $ str "Add Layer (C-a)"
        mkEntry (idx, LayerInfo name vis) =
            if RenameLayer `elem` s^.modes && s^.selectedLayerIndex == idx
               then
                   renderEditor (txt . T.concat) True (s^.layerNameEditor)
               else
                   let applyAttr = if idx == s^.selectedLayerIndex
                                   then withDefAttr selectedLayerAttr
                                   else id
                       showHiddenStatus = if vis then id else (<+> str "H")
                   in clickable (if idx == s^.selectedLayerIndex
                                 then LayerName
                                 else SelectLayer idx) $
                      applyAttr $ vLimit 1 $
                      showHiddenStatus $
                      str name <+> fill ' '
        layerOptions =
            let i = s^.selectedLayerIndex
                entry n label =
                    clickable n $ vLimit 1 $ withDefAttr clickableAttr $
                                             str label <+> fill ' '
            in vBox $ catMaybes
                 [ Just $ hBorderWithLabel (str "Layer Options")
                 , Just $ entry ToggleLayerVisible
                            (if s^.layerInfoFor(i).layerVisible
                             then "Hide (C-v)"
                             else "Show (C-v)")
                 , if i /= head (s^.layerOrder)
                      then Just $ entry MoveLayerUp "Move up (C-u)"
                      else Nothing
                 , if i /= last (s^.layerOrder)
                      then Just $ entry MoveLayerDown "Move down (C-d)"
                      else Nothing
                 , if length (s^.layerOrder) == 1
                      then Nothing
                      else Just $ entry DeleteLayer "Delete (C-x)"
                 ]

toolHud :: AppState -> Widget Name
toolHud s =
    let toolHuds = [ (Freehand, freehandHud)
                   , (FloodFill, floodfillHud)
                   , (Box, boxHud)
                   , (Line, lineHud)
                   , (Eraser, eraserHud)
                   , (Repaint, repaintHud)
                   , (Restyle, restyleHud)
                   , (Eyedropper, eyedropperHud)
                   ]
    in case lookup (s^.tool) toolHuds of
        Nothing -> emptyWidget
        Just f -> f s

freehandHud :: AppState -> Widget Name
freehandHud s = drawChar s

lineHud :: AppState -> Widget Name
lineHud s = drawChar s

eyedropperHud :: AppState -> Widget Name
eyedropperHud s = drawChar s

floodfillHud :: AppState -> Widget Name
floodfillHud s = drawChar s

boxStyleSelectorEntryWidth :: Int
boxStyleSelectorEntryWidth = 18

styleSelectorEntryWidth :: Int
styleSelectorEntryWidth = 12

boxHud :: AppState -> Widget Name
boxHud = drawBoxStyleSelector

eraserHud :: AppState -> Widget Name
eraserHud = drawToolSize

repaintHud :: AppState -> Widget Name
repaintHud = drawToolSize

restyleHud :: AppState -> Widget Name
restyleHud = drawToolSize

drawToolSize :: AppState -> Widget Name
drawToolSize s =
    let inc = clickable IncreaseToolSize $ withDefAttr keybindingAttr $ str ">>"
        dec = clickable DecreaseToolSize $ withDefAttr keybindingAttr $ str "<<"
        sz = fromMaybe (error "BUG: current tool has no size") $ toolSize s
    in borderWithLabel (str "Size") $
       dec <+> (hLimit 5 $ hCenter $ str $ show sz) <+> inc

drawBoxStyleSelector :: AppState -> Widget Name
drawBoxStyleSelector s =
    let styleName = fst $ getBoxBorderStyle s
    in clickable BoxStyleSelector $
       borderWithLabel (str "Box Style") $
       hLimit boxStyleSelectorEntryWidth $
       hCenter $ str styleName

drawStyleSelector :: AppState -> Widget Name
drawStyleSelector s =
    clickable StyleSelector $
    borderWithLabel (str "St" <+> (withDefAttr keybindingAttr $ str "y") <+> str "le") $
    hLimit styleSelectorEntryWidth $
    hCenter $ raw $ V.string (V.defAttr `V.withStyle` (s^.drawStyle)) "demo"

drawCanvasSize :: AppState -> Widget Name
drawCanvasSize s =
    let (width, height) = s^.appCanvasSize
    in clickable ResizeCanvas $
       borderWithLabel (str "Can" <+> (withDefAttr keybindingAttr (str "v")) <+> str "as") $
       hLimit 8 $ hCenter (str $ show width <> "x" <> show height)

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
       (str $ ":" <> toolName (s^.tool))

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
        curColor = drawPaletteEntry s curIdx 4

canvas :: AppState -> Widget Name
canvas s =
    let appLayers = concat
            [ if s^.layerInfoFor(idx).layerVisible
              then if shouldUseOverlay s && idx == s^.selectedLayerIndex
                   then [s^.drawingOverlay, s^.layerAt idx]
                   else [s^.layerAt idx]
              else []
            | idx <- s^.layerOrder
            ]
        sz = s^.appCanvasSize
        widthAdjust = if s^.layerListVisible
                      then 10
                      else 0
    in centerAbout (s^.canvasOffset & _2 %~ pred
                                    & _1 %~ (subtract widthAdjust)) $
       updateAttrMap (applyAttrMappings [(borderAttr, fg V.white)]) $
       setAvailableSize (sz & each %~ (+ 2)) $
       border $
       if null appLayers
          then raw $ V.charFill V.defAttr ' ' (sz^._1) (sz^._2)
          else clickable Canvas $
               raw $ canvasLayersToImage appLayers

shouldUseOverlay :: AppState -> Bool
shouldUseOverlay s =
    isJust $ s^.dragging
