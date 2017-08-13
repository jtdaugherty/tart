module Events.AskToSave
  ( handleAskToSaveEvent
  )
where

import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Data.Text as T

import Brick
import Brick.Widgets.Edit

import Types
import Util

handleAskToSaveEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleAskToSaveEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    halt s
handleAskToSaveEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
    let [fn] = getEditContents (s^.askToSaveFilenameEdit)
    if T.null fn
        then halt s
        else quit False $ s & canvasPath .~ Just (T.unpack fn)
handleAskToSaveEvent s (VtyEvent e) =
    continue =<< handleEventLensed s askToSaveFilenameEdit handleEditorEvent e
handleAskToSaveEvent s _ =
    continue s
