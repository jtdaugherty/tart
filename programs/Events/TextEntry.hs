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

handleTextEntryEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleTextEntryEvent e = do
    result <- handleAttrEvent e
    case result of
        True -> return ()
        False -> handleEvent e

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    -- Commit the text to the drawing and return to main mode
    s <- get
    modify popMode
    drawTextAtPoint (s^.textEntryStart) (s^.textEntered)
handleEvent (VtyEvent (V.EvKey V.KBS [])) = do
    textEntered %= (\t -> if null t then t else init t)
handleEvent (VtyEvent (V.EvKey V.KEsc [])) =
    -- Cancel
    modify popMode
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) | c /= '\t' = do
    -- Enter character
    s <- get
    textEntered %= (<> [(c, currentPaletteAttribute s)])
    start <- use textEntryStart
    ent <- use textEntered
    textEntered .= truncateText start ent s
handleEvent _ =
    -- Ignore everything else
    return ()
