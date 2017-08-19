module Events.TextEntry
  ( handleTextEntryEvent
  )
where

import Brick
import qualified Graphics.Vty as V
import Data.Monoid ((<>))
import Lens.Micro.Platform

import Types
import Util
import Draw

handleTextEntryEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleTextEntryEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
    -- Commit the text to the drawing and return to main mode
    continue =<< drawTextAtPoint (s^.textEntryStart) (s^.textEntered) (setMode Main s)
handleTextEntryEvent s (VtyEvent (V.EvKey V.KBS [])) = do
    continue $ s & textEntered %~ (\t -> if null t then t else init t)
handleTextEntryEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    -- Cancel
    continue $ setMode Main s
handleTextEntryEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | c /= '\t' = do
    -- Enter character
    let s' = s & textEntered %~ (<> [(c, currentPaletteAttribute s)])
    continue $ s' & textEntered .~ truncateText (s'^.textEntryStart) (s'^.textEntered) s'
handleTextEntryEvent s _ =
    -- Ignore everything else
    continue s
