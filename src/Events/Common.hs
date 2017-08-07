module Events.Common
  ( handleCommonEvent
  )
where

import Brick
import qualified Graphics.Vty as V
import Lens.Micro.Platform

import Types
import Util

handleCommonEvent :: AppState -> BrickEvent Name e -> EventM Name (Maybe AppState)
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 't') [])) = do
    if s^.mode == ToolSelect
       then return $ Just $ s & mode .~ Main
       else return $ Just $ beginToolSelect s
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
    if s^.mode == FgPaletteEntrySelect
       then return $ Just $ s & mode .~ Main
       else return $ Just $ beginFgPaletteSelect s
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) = do
    if s^.mode == BgPaletteEntrySelect
       then return $ Just $ s & mode .~ Main
       else return $ Just $ beginBgPaletteSelect s
handleCommonEvent _ _ = return Nothing
