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

handleAskForSaveFilenameEvent :: Bool -> AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleAskForSaveFilenameEvent isQuitting s (VtyEvent (V.EvKey V.KEsc [])) =
    if isQuitting then halt s else continue $ popMode s
handleAskForSaveFilenameEvent isQuitting s (VtyEvent (V.EvKey V.KEnter [])) = do
    let [fn] = getEditContents (s^.askToSaveFilenameEdit)
    if T.null fn
        then if isQuitting then halt s else continue $ popMode s
        else let s' = s & canvasPath .~ Just (T.unpack fn)
             in if isQuitting
                then quit False (popMode s')
                else continue =<< (popMode <$> saveAndContinue s')
handleAskForSaveFilenameEvent _ s (VtyEvent e) =
    continue =<< handleEventLensed s askToSaveFilenameEdit handleEditorEvent e
handleAskForSaveFilenameEvent _ s _ =
    continue s

