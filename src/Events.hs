module Events
  ( handleEvent
  )
where

import Brick
import Lens.Micro.Platform
import qualified Graphics.Vty as V

import Types
import Events.Main
import Events.CharacterSelect
import Events.PaletteEntrySelect
import Events.ToolSelect
import Util

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvResize _ _)) = do
    s' <- updateExtents s
    continue =<< resizeCanvas s'
handleEvent s e = do
    s' <- updateExtents s
    case s'^.mode of
        Main                 -> handleMainEvent s' e
        FgPaletteEntrySelect -> handlePaletteEntrySelectEvent s' e
        BgPaletteEntrySelect -> handlePaletteEntrySelectEvent s' e
        ToolSelect           -> handleToolSelectEvent s' e
        CharacterSelect      -> handleCharacterSelectEvent s' e

updateExtents :: AppState -> EventM Name AppState
updateExtents s = do
    fgExtent <- lookupExtent FgSelector
    bgExtent <- lookupExtent BgSelector
    tsExtent <- lookupExtent ToolSelector

    return $ s & fgPaletteSelectorExtent .~ fgExtent
               & bgPaletteSelectorExtent .~ bgExtent
               & toolSelectorExtent      .~ tsExtent
