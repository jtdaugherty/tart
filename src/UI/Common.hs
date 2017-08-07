module UI.Common
  ( drawPaletteEntry
  , drawPalette
  )
where

import Brick
import Brick.Widgets.Border
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

import Types

drawPaletteEntry :: Vec.Vector V.Color -> Int -> Widget Name
drawPaletteEntry pal idx =
    let color = Vec.unsafeIndex pal idx
    in raw $ V.string (V.defAttr `V.withBackColor` color) "  "

drawPalette :: Vec.Vector V.Color -> (Int -> Name) -> Extent Name -> Widget Name
drawPalette pal mkName ext =
    translateBy l $
    border $ vBox entries
    where
        l = Location ( fst $ loc $ extentUpperLeft ext
                     , (snd $ extentSize ext) + (snd $ loc $ extentUpperLeft ext)
                     )
        idxs = [0..Vec.length pal-1]
        entries = mkEntry <$> idxs
        mkEntry i = clickable (mkName i) $ drawPaletteEntry pal i
