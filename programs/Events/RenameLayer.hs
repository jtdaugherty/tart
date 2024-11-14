module Events.RenameLayer
  ( handleRenameLayerEvent
  )
where

import Brick
import Brick.Widgets.Edit
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Lens.Micro.Platform

import Types
import State

handleRenameLayerEvent :: BrickEvent Name AppEvent
                       -> EventM Name AppState ()
handleRenameLayerEvent (VtyEvent (V.EvKey V.KEsc [])) =
    modify popMode
handleRenameLayerEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    ed <- use layerNameEditor
    renameCurrentLayer (T.concat $ getEditContents ed)
handleRenameLayerEvent e =
    zoom layerNameEditor $ handleEditorEvent e
handleRenameLayerEvent _ =
    return ()
