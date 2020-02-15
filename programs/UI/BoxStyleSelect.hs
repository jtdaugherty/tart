module UI.BoxStyleSelect
  ( drawBoxStyleSelectUI
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Lens.Micro.Platform

import Types
import UI.Main
import State

drawBoxStyleSelectUI :: AppState -> [Widget Name]
drawBoxStyleSelectUI s =
    let Just ext = s^.boxStyleSelectorExtent
        toolSel = drawBoxStyleSelector ext
    in toolSel

drawBoxStyleSelector :: Extent Name -> [Widget Name]
drawBoxStyleSelector ext =
    [borderHack, body]
    where
        borderHack = translateBy l bottomBorder
        l = Location ( fst $ loc $ extentUpperLeft ext
                     , (snd $ extentSize ext) + (snd $ loc $ extentUpperLeft ext) - 1
                     )
        bottomBorder = hBox [ borderElem bsIntersectL
                            , hLimit boxStyleSelectorEntryWidth hBorder
                            , borderElem bsIntersectR
                            ]
        body = translateBy l $
               border $ vBox entries
        entries = mkEntry <$> zip [0..] boxStyles
        mkEntry (i, (n, _)) =
            clickable (BoxStyleSelectorEntry i) $
            vLimit 1 $
            hLimit boxStyleSelectorEntryWidth $
            (txt n) <+> fill ' '
