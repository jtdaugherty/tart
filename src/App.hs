module App
  ( application
  , mkInitialState
  )
where

import qualified Graphics.Vty as V

import Brick

import Types
import Events
import UI
import Util

mkInitialState :: AppState
mkInitialState =
    AppState { _drawing       = mempty
             , _canvasSize    = (0, 0)
             , _mode          = Main
             , _tool          = Point
             , _showHud       = True
             }

application :: App AppState () Name
application =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = \s -> do
            vty <- getVtyHandle
            V.setMode (V.outputIface vty) V.Mouse True
            resizeCanvas s
        , appAttrMap = const $ attrMap V.defAttr []
        }
