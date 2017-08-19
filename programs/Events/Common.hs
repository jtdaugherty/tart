module Events.Common
  ( handleCommonEvent
  )
where

import Brick
import qualified Graphics.Vty as V

import Types
import Util

handleCommonEvent :: AppState -> BrickEvent Name e -> EventM Name (Maybe AppState)
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 't') [])) = do
    if currentMode s == ToolSelect
       then return $ Just $ popMode s
       else return $ Just $ beginToolSelect s
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
    if currentMode s == FgPaletteEntrySelect
       then return $ Just $ popMode s
       else return $ Just $ beginFgPaletteSelect s
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) = do
    if currentMode s == BgPaletteEntrySelect
       then return $ Just $ popMode s
       else return $ Just $ beginBgPaletteSelect s
handleCommonEvent _ _ = return Nothing
