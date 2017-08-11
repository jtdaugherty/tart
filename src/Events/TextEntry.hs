module Events.TextEntry
  ( handleTextEntryEvent
  )
where

import Brick
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Lens.Micro.Platform

import Types
import Util
import Draw

handleTextEntryEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleTextEntryEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
    -- Commit the text to the drawing and return to main mode
    continue =<< drawTextAtPoint (s^.textEntryStart) (setMode Main s)
handleTextEntryEvent s (VtyEvent (V.EvKey V.KBS [])) = do
    continue $ s & textEntered %~ (\t -> if T.null t then t else T.init t)
handleTextEntryEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    -- Cancel
    continue $ setMode Main s
handleTextEntryEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | c /= '\t' = do
    -- Enter character
    let s' = s & textEntered %~ (`T.snoc` c)
    continue $ s' & textEntered .~ truncateEnteredText s'
handleTextEntryEvent s _ =
    -- Ignore everything else
    continue s
