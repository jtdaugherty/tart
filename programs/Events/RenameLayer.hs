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
import Util

handleRenameLayerEvent :: AppState
                       -> BrickEvent Name AppEvent
                       -> EventM Name (Next AppState)
handleRenameLayerEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    continue $ popMode s
handleRenameLayerEvent s (VtyEvent (V.EvKey V.KEnter [])) =
    continue $ renameCurrentLayer (T.concat $ getEditContents $ s^.layerNameEditor) s
handleRenameLayerEvent s (VtyEvent e) =
    continue =<< handleEventLensed s layerNameEditor handleEditorEvent e
handleRenameLayerEvent s _ =
    continue s
