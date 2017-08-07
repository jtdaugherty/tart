module UI.Common
  ( drawPaletteEntry
  , drawPalette
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

import Types

drawPaletteEntry :: Vec.Vector V.Color -> Int -> Int -> Widget Name
drawPaletteEntry pal idx width =
    let color = Vec.unsafeIndex pal idx
    in raw $ V.string (V.defAttr `V.withBackColor` color) (replicate width ' ')

drawPalette :: Vec.Vector V.Color -> (Int -> Name) -> Extent Name -> [Widget Name]
drawPalette pal mkName ext =
    [borderHack, body]
    where
        borderHack = translateBy l topBorder
        topBorder = hBox [ borderElem bsIntersectL
                         , hLimit 6 hBorder
                         , borderElem bsIntersectR
                         ]
        body = translateBy l $ border $ vBox entries
        l = Location ( fst $ loc $ extentUpperLeft ext
                     , (snd $ extentSize ext) + (snd $ loc $ extentUpperLeft ext) - 1
                     )
        idxs = [0..Vec.length pal-1]
        entries = mkEntry <$> idxs
        mkEntry i = clickable (mkName i) $ drawPaletteEntry pal i 6
