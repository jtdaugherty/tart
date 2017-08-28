{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module App
  ( application
  , mkInitialState
  )
where

import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Map as M
import Lens.Micro.Platform

import Brick
import Brick.BChan (BChan)
import Brick.Focus
import Brick.Widgets.Edit (editor)

import Types
import Events
import UI
import Theme
import Tart.Canvas

defaultPalette :: Vec.Vector (Maybe V.Color)
defaultPalette = Vec.fromList
    [ Nothing -- default attribute
    , Just V.white
    , Just V.brightWhite
    , Just V.black
    , Just V.brightBlack
    , Just V.blue
    , Just V.brightBlue
    , Just V.red
    , Just V.brightRed
    , Just V.magenta
    , Just V.brightMagenta
    , Just V.green
    , Just V.brightGreen
    , Just V.cyan
    , Just V.brightCyan
    , Just V.yellow
    , Just V.brightYellow
    ]

initialCanvasSize :: (Int, Int)
initialCanvasSize = (40, 17)

mkInitialState :: BChan AppEvent
               -> Maybe (Maybe FilePath, [Canvas], [Int], [String])
               -> IO AppState
mkInitialState chan mc = do
    (cs, order, names, fp) <- case mc of
        Nothing -> do
            c <- newCanvas initialCanvasSize
            return ([c], [0], ["default"], Nothing)
        Just (fp, cs, order, names) ->
            return (cs, order, names, fp)

    let sz = canvasSize $ cs !! 0
    overlay <- newCanvas sz

    return $ AppState { _layers                  = M.fromList $ zip [0..] cs
                      , _layerInfo               = M.fromList $ zip [0..] (LayerInfo <$> names <*> pure True)
                      , _layerOrder              = order
                      , _drawingOverlay          = overlay
                      , _selectedLayerIndex      = head order
                      , _modes                   = [Main]
                      , _tool                    = Freehand
                      , _appCanvasSize           = sz
                      , _drawCharacter           = '*'
                      , _drawFgPaletteIndex      = 0
                      , _drawBgPaletteIndex      = 0
                      , _palette                 = defaultPalette
                      , _fgPaletteSelectorExtent = Nothing
                      , _bgPaletteSelectorExtent = Nothing
                      , _toolSelectorExtent      = Nothing
                      , _boxStyleSelectorExtent  = Nothing
                      , _styleSelectorExtent     = Nothing
                      , _canvasExtent            = Nothing
                      , _dragging                = Nothing
                      , _layerNameEditor         = editor LayerNameEditor (Just 1) ""
                      , _canvasSizeWidthEdit     = editor CanvasSizeWidthEdit (Just 1) ""
                      , _canvasSizeHeightEdit    = editor CanvasSizeHeightEdit (Just 1) ""
                      , _canvasSizeFocus         = focusRing [ CanvasSizeWidthEdit
                                                             , CanvasSizeHeightEdit
                                                             ]
                      , _canvasOffset            = Location $ sz & each %~ (`div` 2)
                      , _canvasPath              = fp
                      , _canvasDirty             = False
                      , _askToSaveFilenameEdit   = editor AskToSaveFilenameEdit (Just 1) ""
                      , _appEventChannel         = chan
                      , _textEntered             = mempty
                      , _textEntryStart          = (0, 0)
                      , _boxStyleIndex           = 0
                      , _eraserSize              = 1
                      , _repaintSize             = 1
                      , _restyleSize             = 1
                      , _undoStack               = []
                      , _redoStack               = []
                      , _drawStyle               = noStyle
                      }

application :: App AppState AppEvent Name
application =
    App { appDraw = drawUI
        , appChooseCursor = \s locs ->
            if | CanvasSizePrompt `elem` s^.modes -> do
                   cur <- focusGetCurrent (s^.canvasSizeFocus)
                   showCursorNamed cur locs
               | AskToSave `elem` s^.modes ->
                   showCursorNamed AskToSaveFilenameEdit locs
               | TextEntry `elem` s^.modes ->
                   showCursorNamed TextEntryCursor locs
               | RenameLayer `elem` s^.modes ->
                   showCursorNamed LayerNameEditor locs
               | otherwise -> Nothing
        , appHandleEvent = handleEvent
        , appStartEvent = \s -> do
            vty <- getVtyHandle
            V.setMode (V.outputIface vty) V.Mouse True
            V.setMode (V.outputIface vty) V.BracketedPaste True
            return s
        , appAttrMap = const theme
        }
