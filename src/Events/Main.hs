module Events.Main
  ( handleMainEvent
  )
where

import Brick
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import qualified Data.Array.MArray as A
import Control.Monad.Trans (liftIO)

import Types

handleMainEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleMainEvent s (MouseDown Canvas _ _ (Location l)) = do
    newFrozen <- liftIO $ do
        let arr = s^.drawing
        A.writeArray arr l 'x'
        A.freeze arr
    continue $ s & drawingFrozen .~ newFrozen
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleMainEvent s _ = continue s
