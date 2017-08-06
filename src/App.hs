module App
  ( application
  , mkInitialState
  )
where

import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

import Brick

import Types
import Events
import UI
import Util

defaultPalette :: Vec.Vector V.Color
defaultPalette = Vec.fromList
    [ V.white
    , V.blue
    , V.red
    , V.magenta
    , V.green
    , V.cyan
    , V.yellow
    , V.black
    ]

mkInitialState :: AppState
mkInitialState =
    AppState { _drawing          = mempty
             , _canvasSize       = (0, 0)
             , _mode             = Main
             , _tool             = FreeHand
             , _drawCharacter    = '*'
             , _showHud          = True
             , _drawPaletteIndex = 0
             , _palette          = defaultPalette
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
