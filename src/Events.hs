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
import Events.CanvasSizePrompt
import Events.AskToSave

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvResize _ _)) = do
    continue =<< updateExtents s
handleEvent s e = do
    s' <- updateExtents s

    let next = case e of
          MouseDown n _ _ l ->
              case s'^.dragging of
                  Nothing ->
                      Just (e, s' & dragging .~ Just (n, l, l))
                  Just (n', start, _) | n == n' ->
                      Just (e, s' & dragging .~ Just (n, start, l))
                  _ -> Nothing
          MouseUp _ _ _ ->
              Just (e, s' & dragging .~ Nothing)
          _ ->
              Just (e, s')

    case next of
        Nothing -> continue s'
        Just (ev, st) ->
            case st^.mode of
                Main                 -> handleMainEvent st ev
                FgPaletteEntrySelect -> handlePaletteEntrySelectEvent st ev
                BgPaletteEntrySelect -> handlePaletteEntrySelectEvent st ev
                ToolSelect           -> handleToolSelectEvent st ev
                CharacterSelect      -> handleCharacterSelectEvent st ev
                CanvasSizePrompt     -> handleCanvasSizePromptEvent st ev
                AskToSave            -> handleAskToSaveEvent st ev

updateExtents :: AppState -> EventM Name AppState
updateExtents s = do
    fgExtent <- lookupExtent FgSelector
    bgExtent <- lookupExtent BgSelector
    tsExtent <- lookupExtent ToolSelector

    return $ s & fgPaletteSelectorExtent .~ fgExtent
               & bgPaletteSelectorExtent .~ bgExtent
               & toolSelectorExtent      .~ tsExtent
