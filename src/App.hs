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
import Theme

defaultPalette :: Vec.Vector PaletteEntry
defaultPalette = Vec.fromList
    [ PaletteEntry id id
    , PaletteEntry (`V.withForeColor` V.white   ) (`V.withBackColor` V.white)
    , PaletteEntry (`V.withForeColor` V.black   ) (`V.withBackColor` V.black)
    , PaletteEntry (`V.withForeColor` V.blue    ) (`V.withBackColor` V.blue)
    , PaletteEntry (`V.withForeColor` V.red     ) (`V.withBackColor` V.red)
    , PaletteEntry (`V.withForeColor` V.magenta ) (`V.withBackColor` V.magenta)
    , PaletteEntry (`V.withForeColor` V.green   ) (`V.withBackColor` V.green)
    , PaletteEntry (`V.withForeColor` V.cyan    ) (`V.withBackColor` V.cyan)
    , PaletteEntry (`V.withForeColor` V.yellow  ) (`V.withBackColor` V.yellow)
    ]

mkInitialState :: AppState
mkInitialState =
    AppState { _drawing                 = mempty
             , _canvasSize              = (0, 0)
             , _mode                    = Main
             , _tool                    = FreeHand
             , _drawCharacter           = '*'
             , _showHud                 = True
             , _drawFgPaletteIndex      = 0
             , _drawBgPaletteIndex      = 0
             , _palette                 = defaultPalette
             , _fgPaletteSelectorExtent = Nothing
             , _bgPaletteSelectorExtent = Nothing
             , _toolSelectorExtent      = Nothing
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
        , appAttrMap = const theme
        }
