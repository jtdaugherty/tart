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
handleMainEvent s (MouseDown Canvas _ _ (Location l)) = do
    continue $ drawAtPoint l s
handleMainEvent s (MouseDown (FgPaletteEntry idx) _ _ _) = do
    continue $ setFgPaletteIndex s idx
handleMainEvent s (MouseDown (BgPaletteEntry idx) _ _ _) = do
    continue $ setBgPaletteIndex s idx
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'h') [])) =
    continue $ toggleHud s
handleMainEvent s (VtyEvent (V.EvKey (V.KChar '1') [])) =
    continue $ setTool s FreeHand
handleMainEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) =
    continue $ beginCharacterSelect s
handleMainEvent s _ = continue s
