module UI.TextEntry
  ( drawTextEntryUI
  )
where

import Brick
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Types
import Draw

drawTextEntryUI :: AppState -> [Widget Name]
drawTextEntryUI s = [textOverlay s]

textOverlay :: AppState -> Widget Name
textOverlay s =
    let Just cExt = s^.canvasExtent
        p = s^.textEntryStart
        t = s^.textEntered
        off = extentUpperLeft cExt & _1 %~ (+ (p^._1))
                                   & _2 %~ (+ (p^._2))
    in translateBy off $
       showCursor TextEntryCursor (Location (length t, 0)) $
       (raw $ V.horizCat $ (\(ch, attr) -> V.char attr ch) <$>
           (truncateText (s^.textEntryStart) (s^.textEntered) s))
