module Events
  ( handleEvent
  )
where

import Brick
import Brick.BChan (writeBChan)
import Control.Monad.Trans (liftIO)
import Lens.Micro.Platform
import qualified Graphics.Vty as V

import Types
import Events.Main
import Events.CharacterSelect
import Events.PaletteEntrySelect
import Events.ToolSelect
import Events.CanvasSizePrompt
import Events.AskToSave
import Events.TextEntry
import Events.BoxStyleSelect
import Events.StyleSelect
import Events.RenameLayer

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvResize _ _)) = do
    continue =<< updateExtents s
handleEvent s e = do
    s' <- updateExtents s

    next <- case e of
          MouseDown n _ _ l ->
              case s'^.dragging of
                  Nothing ->
                      return $ Just (e, s' & dragging .~ Just (n, l, l))
                  Just (n', start, _) | n == n' ->
                      return $ Just (e, s' & dragging .~ Just (n, start, l))
                  _ ->
                      return $ Nothing
          MouseUp _ _ _ -> do
              case s'^.dragging of
                  Nothing -> return ()
                  Just (n, l0, l1) -> do
                      let ev = DragFinished n l0 l1
                      liftIO $ writeBChan (s^.appEventChannel) ev
              return $ Just (e, s' & dragging .~ Nothing)
          _ ->
              return $ Just (e, s')

    case next of
        Nothing -> continue s'
        Just (ev, st) ->
            case currentMode st of
                Main                 -> handleMainEvent st ev
                FgPaletteEntrySelect -> handlePaletteEntrySelectEvent st ev
                BgPaletteEntrySelect -> handlePaletteEntrySelectEvent st ev
                ToolSelect           -> handleToolSelectEvent st ev
                CharacterSelect      -> handleCharacterSelectEvent st ev
                CanvasSizePrompt     -> handleCanvasSizePromptEvent st ev
                AskToSave            -> handleAskToSaveEvent st ev
                TextEntry            -> handleTextEntryEvent st ev
                BoxStyleSelect       -> handleBoxStyleSelectEvent st ev
                StyleSelect          -> handleStyleSelectEvent st ev
                RenameLayer          -> handleRenameLayerEvent st ev

updateExtents :: AppState -> EventM Name AppState
updateExtents s = do
    fgExtent <- lookupExtent FgSelector
    bgExtent <- lookupExtent BgSelector
    tsExtent <- lookupExtent ToolSelector
    cExtent <- lookupExtent Canvas
    bsExtent <- lookupExtent BoxStyleSelector
    ssExtent <- lookupExtent StyleSelector

    return $ s & fgPaletteSelectorExtent .~ fgExtent
               & bgPaletteSelectorExtent .~ bgExtent
               & toolSelectorExtent      .~ tsExtent
               & canvasExtent            .~ cExtent
               & boxStyleSelectorExtent  .~ bsExtent
               & styleSelectorExtent     .~ ssExtent
