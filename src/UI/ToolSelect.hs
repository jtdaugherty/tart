module UI.ToolSelect
  ( drawToolSelectUI
  )
where

import Data.Monoid ((<>))
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Lens.Micro.Platform

import Types
import UI.Main

drawToolSelectUI :: AppState -> [Widget Name]
drawToolSelectUI s =
    let Just ext = s^.toolSelectorExtent
        toolSel = drawToolSelector ext
    in toolSel <> drawMainUI s

tools :: [(Tool, Int)]
tools =
    [ (FreeHand, 1)
    , (Eraser, 0)
    ]

drawToolSelector :: Extent Name -> [Widget Name]
drawToolSelector ext =
    [borderHack, body]
    where
        borderHack = translateBy l topBorder
        topBorder = hBox [ borderElem bsIntersectL
                         , hLimit toolSelectorEntryWidth hBorder
                         , borderElem bsIntersectR
                         ]
        body = translateBy l $ border $ vBox entries
        l = Location ( fst $ loc $ extentUpperLeft ext
                     , (snd $ extentSize ext) + (snd $ loc $ extentUpperLeft ext) - 1
                     )
        entries = mkEntry <$> tools
        mkEntry (t, i) =
            clickable (ToolSelectorEntry t) $
            vLimit 1 $
            hLimit toolSelectorEntryWidth $
            (str $ show i <> ":" <> show t) <+> fill ' '
