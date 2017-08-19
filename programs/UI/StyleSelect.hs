module UI.StyleSelect
  ( drawStyleSelectUI
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Lens.Micro.Platform
import qualified Graphics.Vty as V

import Types
import UI.Main
import Util
import Theme

drawStyleSelectUI :: AppState -> [Widget Name]
drawStyleSelectUI s =
    let Just ext = s^.styleSelectorExtent
        stySel = drawStyleSelector (s^.drawStyle) ext
    in stySel

drawStyleSelector :: V.Style -> Extent Name -> [Widget Name]
drawStyleSelector curStyle ext =
    [borderHack, body]
    where
        borderHack = translateBy l bottomBorder
        l = Location ( fst $ loc $ extentUpperLeft ext
                     , (snd $ extentSize ext) + (snd $ loc $ extentUpperLeft ext) - 1
                     )
        bottomBorder = hBox [ borderElem bsIntersectL
                            , hLimit styleSelectorEntryWidth hBorder
                            , borderElem bsIntersectR
                            ]
        body = translateBy l $
               border $ vBox entries
        entries = mkEntry <$> styleBindings
        maybeActive sty =
            if hasStyle sty curStyle
            then (<+> str "*")
            else id
        mkEntry (ch, sty) =
            clickable (StyleSelectorEntry sty) $
            vLimit 1 $
            hLimit styleSelectorEntryWidth $
            (withDefAttr keybindingAttr (str [ch])) <+> str ":" <+>
            (maybeActive sty $ raw $ V.string (V.defAttr `V.withStyle` sty) "demo") <+>
            fill ' '
