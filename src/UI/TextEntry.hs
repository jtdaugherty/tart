module UI.TextEntry
  ( drawTextEntryUI
  )
where

import Brick
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import UI.Main
import Types
import Util
import Draw

drawTextEntryUI :: AppState -> [Widget Name]
drawTextEntryUI s =
    textOverlay s : drawMainUI s

textOverlay :: AppState -> Widget Name
textOverlay s =
    let Just cExt = s^.canvasExtent
        p = s^.textEntryStart
        t = s^.textEntered
        off = extentUpperLeft cExt & _1 %~ (+ (p^._1))
                                   & _2 %~ (+ (p^._2))
    in translateBy off $
       showCursor TextEntryCursor (Location (T.length t, 0)) $
       (raw $ V.string (currentPaletteAttribute s) (T.unpack (truncateEnteredText s)))
