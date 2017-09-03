module Events.AskForSaveFilename
  ( handleAskForSaveFilenameEvent
  )
where

import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Data.Text as T

import Brick
import Brick.Widgets.Edit

import Types
import State

handleAskForSaveFilenameEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleAskForSaveFilenameEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    halt s
handleAskForSaveFilenameEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
    let [fn] = getEditContents (s^.askToSaveFilenameEdit)
    if T.null fn
        then halt s
        else quit False $ s & canvasPath .~ Just (T.unpack fn)
handleAskForSaveFilenameEvent s (VtyEvent e) =
    continue =<< handleEventLensed s askToSaveFilenameEdit handleEditorEvent e
handleAskForSaveFilenameEvent s _ =
    continue s

