module Events.Common
  ( handleCommonEvent
  )
where

import Brick
import qualified Graphics.Vty as V

import Types
import State

handleCommonEvent :: BrickEvent Name e -> EventM Name AppState Bool
handleCommonEvent (VtyEvent (V.EvKey (V.KChar 't') [])) = do
    s <- get
    if currentMode s == ToolSelect
       then modify popMode
       else beginToolSelect
    return True
handleCommonEvent (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
    s <- get
    if currentMode s == FgPaletteEntrySelect
       then modify popMode
       else beginFgPaletteSelect
    return True
handleCommonEvent (VtyEvent (V.EvKey (V.KChar 'b') [])) = do
    s <- get
    if currentMode s == BgPaletteEntrySelect
       then modify popMode
       else beginBgPaletteSelect
    return True
handleCommonEvent _ = return False
