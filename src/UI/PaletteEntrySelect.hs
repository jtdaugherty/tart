module UI.PaletteEntrySelect
  ( drawPaletteEntrySelectUI
  )
where

import Data.Monoid ((<>))
import Brick
import Lens.Micro.Platform

import Types
import UI.Main
import UI.Common

drawPaletteEntrySelectUI :: AppState -> [Widget Name]
drawPaletteEntrySelectUI s =
    let (Just ext, mkName) = case s^.mode of
          FgPaletteEntrySelect -> (s^.fgPaletteSelectorExtent, FgPaletteEntry)
          BgPaletteEntrySelect -> (s^.bgPaletteSelectorExtent, BgPaletteEntry)
          m -> error $ "BUG: should never get called in mode " <> show m
        pal = drawPalette (s^.palette) mkName ext
    in pal <> drawMainUI s
