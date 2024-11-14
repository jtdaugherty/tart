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

handleAskForSaveFilenameEvent :: Bool -> BrickEvent Name e -> EventM Name AppState ()
handleAskForSaveFilenameEvent isQuitting (VtyEvent (V.EvKey V.KEsc [])) =
    if isQuitting then halt else modify popMode
handleAskForSaveFilenameEvent isQuitting (VtyEvent (V.EvKey V.KEnter [])) = do
    editor <- use askToSaveFilenameEdit
    let [fn] = getEditContents editor
    if T.null fn
        then if isQuitting then halt else modify popMode
        else do
            canvasPath .= Just (T.unpack fn)
            if isQuitting
                then modify popMode >> quit False
                else modify popMode >> saveAndContinue
handleAskForSaveFilenameEvent _ e = do
    zoom askToSaveFilenameEdit $ handleEditorEvent e
handleAskForSaveFilenameEvent _ _ =
    return ()

