module Events.TextEntry
  ( handleTextEntryEvent
  )
where

import Brick
import qualified Graphics.Vty as V
import Data.Monoid ((<>))
import Lens.Micro.Platform

import Types
import State
import Draw
import Events.Main

handleTextEntryEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleTextEntryEvent s e = do
    result <- handleAttrEvent s e
    case result of
        Just s' -> continue s'
        Nothing -> handleEvent s e

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
    -- Commit the text to the drawing and return to main mode
    continue =<< drawTextAtPoint (s^.textEntryStart) (s^.textEntered) (popMode s)
handleEvent s (VtyEvent (V.EvKey V.KBS [])) = do
    continue $ s & textEntered %~ (\t -> if null t then t else init t)
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) =
    -- Cancel
    continue $ popMode s
handleEvent s (VtyEvent (V.EvKey (V.KChar c) [])) | c /= '\t' = do
    -- Enter character
    let s' = s & textEntered %~ (<> [(c, currentPaletteAttribute s)])
    continue $ s' & textEntered .~ truncateText (s'^.textEntryStart) (s'^.textEntered) s'
handleEvent s _ =
    -- Ignore everything else
    continue s
