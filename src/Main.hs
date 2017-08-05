module Main where

import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Data.Array.MArray as A

import Brick

import Types
import Events
import UI
import Util

app :: App AppState () Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = \s -> do
              vty <- getVtyHandle
              V.setMode (V.outputIface vty) V.Mouse True
              resizeCanvas s
          , appAttrMap = const $ attrMap V.defAttr []
          }

mkInitialState :: IO AppState
mkInitialState = do
    let arrayBounds = ((0, 0), (0, 0))
    draw <- A.newArray arrayBounds blankCharacter
    return $ AppState { _drawing = draw
                      , _canvasSize = (0, 0)
                      }

main :: IO ()
main = do
    checkForMouseSupport
    st <- mkInitialState
    void $ defaultMain app st
