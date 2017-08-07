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
    return $ Just $ beginToolSelect s
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 'f') [])) = do
    return $ Just $ beginFgPaletteSelect s
handleCommonEvent s (VtyEvent (V.EvKey (V.KChar 'b') [])) = do
    return $ Just $ beginBgPaletteSelect s
handleCommonEvent _ _ = return Nothing
