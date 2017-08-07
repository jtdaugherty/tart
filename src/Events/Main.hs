module Events.Main
  ( handleMainEvent
  )
where

import Brick
import qualified Graphics.Vty as V

import Types
import Draw
import Util

handleMainEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleMainEvent s (MouseDown FgSelector _ _ _) = do
    continue $ beginFgPaletteSelect s
handleMainEvent s (MouseDown BgSelector _ _ _) = do
    continue $ beginBgPaletteSelect s
handleMainEvent s (MouseDown Canvas _ _ (Location l)) = do
    continue $ drawWithCurrentTool l s
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'h') [])) =
    continue $ toggleHud s
handleMainEvent s (VtyEvent (V.EvKey (V.KChar '1') [])) =
    continue $ setTool s FreeHand
handleMainEvent s (VtyEvent (V.EvKey (V.KChar '0') [])) =
    continue $ setTool s Eraser
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) =
    continue $ beginCharacterSelect s
handleMainEvent s _ = continue s
