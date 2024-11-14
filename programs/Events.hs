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
import Events.AskForSaveFilename
import Events.TextEntry
import Events.BoxStyleSelect
import Events.StyleSelect
import Events.RenameLayer

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvResize _ _)) = do
    updateExtents
handleEvent e = do
    updateExtents
    drg <- use dragging
    chan <- use appEventChannel

    next <- case e of
          MouseDown n _ _ l ->
              case drg of
                  Nothing ->
                      return $ Just (e, dragging .= Just (n, l, l))
                  Just (n', start, _) | n == n' ->
                      return $ Just (e, dragging .= Just (n, start, l))
                  _ ->
                      return Nothing
          MouseUp _ _ _ -> do
              case drg of
                  Nothing -> return ()
                  Just (n, l0, l1) -> do
                      let ev = DragFinished n l0 l1
                      liftIO $ writeBChan chan ev
              return $ Just (e, dragging .= Nothing)
          _ ->
              return $ Just (e, return ())

    case next of
        Nothing -> return ()
        Just (ev, act) -> do
            act
            m <- gets currentMode
            case m of
                Main                 -> handleMainEvent ev
                FgPaletteEntrySelect -> handlePaletteEntrySelectEvent ev
                BgPaletteEntrySelect -> handlePaletteEntrySelectEvent ev
                ToolSelect           -> handleToolSelectEvent ev
                CharacterSelect      -> handleCharacterSelectEvent ev
                CanvasSizePrompt     -> handleCanvasSizePromptEvent ev
                AskToSave            -> handleAskToSaveEvent ev
                AskForSaveFilename q -> handleAskForSaveFilenameEvent q ev
                TextEntry            -> handleTextEntryEvent ev
                BoxStyleSelect       -> handleBoxStyleSelectEvent ev
                StyleSelect          -> handleStyleSelectEvent ev
                RenameLayer          -> handleRenameLayerEvent ev

updateExtents :: EventM Name AppState ()
updateExtents = do
    fgExtent <- lookupExtent FgSelector
    bgExtent <- lookupExtent BgSelector
    tsExtent <- lookupExtent ToolSelector
    cExtent <- lookupExtent Canvas
    bsExtent <- lookupExtent BoxStyleSelector
    ssExtent <- lookupExtent StyleSelector

    fgPaletteSelectorExtent .= fgExtent
    bgPaletteSelectorExtent .= bgExtent
    toolSelectorExtent      .= tsExtent
    canvasExtent            .= cExtent
    boxStyleSelectorExtent  .= bsExtent
    styleSelectorExtent     .= ssExtent
