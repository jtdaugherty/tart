{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord
  , Pixel
  , Tool(..)
  , AppEvent(..)
  , toolName

  , AppState(..)
  , drawing
  , drawingOverlay
  , mode
  , tool
  , drawFgPaletteIndex
  , drawBgPaletteIndex
  , palette
  , drawCharacter
  , fgPaletteSelectorExtent
  , bgPaletteSelectorExtent
  , toolSelectorExtent
  , boxStyleSelectorExtent
  , canvasExtent
  , dragging
  , canvasSizeWidthEdit
  , canvasSizeHeightEdit
  , canvasSizeFocus
  , canvasOffset
  , canvasPath
  , canvasDirty
  , askToSaveFilenameEdit
  , appEventChannel
  , textEntered
  , textEntryStart
  , boxStyleIndex
  )
where

import Brick (Extent, Location)
import Brick.BChan (BChan)
import Brick.Focus
import Brick.Widgets.Edit (Editor)
import qualified Data.Text as T
import Lens.Micro.TH
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Tart.Canvas

data AppEvent =
    DragFinished Name Location Location
    deriving (Eq)

data Mode = Main
          | CharacterSelect
          | FgPaletteEntrySelect
          | BgPaletteEntrySelect
          | ToolSelect
          | BoxStyleSelect
          | CanvasSizePrompt
          | AskToSave
          | TextEntry
          deriving (Eq, Show)

data Name = Canvas
          | TopHud
          | BottomHud
          | ToolSelector
          | ToolSelectorEntry Tool
          | CharSelector
          | FgSelector
          | BgSelector
          | FgPaletteEntry Int
          | BgPaletteEntry Int
          | BoxStyleSelectorEntry Int
          | ResizeCanvas
          | CanvasSizeWidthEdit
          | CanvasSizeHeightEdit
          | AskToSaveFilenameEdit
          | TextEntryCursor
          | BoxStyleSelector
          deriving (Eq, Show, Ord)

data Tool = Freehand
          | Box
          | Recolor
          | Eyedropper
          | FloodFill
          | Eraser
          | TextString
          deriving (Eq, Show, Ord)

toolName :: Tool -> String
toolName Freehand   = "Freehand"
toolName Box        = "Box"
toolName Recolor    = "Re-color"
toolName Eraser     = "Eraser"
toolName Eyedropper = "Eyedropper"
toolName FloodFill  = "Flood fill"
toolName TextString = "Text string"

type Coord = (Int, Int)

type Pixel = (Char, V.Attr)

data AppState =
    AppState { _drawing                 :: Canvas
             , _drawingOverlay          :: Canvas
             , _mode                    :: Mode
             , _drawFgPaletteIndex      :: Int
             , _drawBgPaletteIndex      :: Int
             , _drawCharacter           :: Char
             , _tool                    :: Tool
             , _palette                 :: Vec.Vector (Maybe V.Color)
             , _fgPaletteSelectorExtent :: Maybe (Extent Name)
             , _bgPaletteSelectorExtent :: Maybe (Extent Name)
             , _toolSelectorExtent      :: Maybe (Extent Name)
             , _boxStyleSelectorExtent  :: Maybe (Extent Name)
             , _canvasExtent            :: Maybe (Extent Name)
             , _dragging                :: Maybe (Name, Location, Location)
             , _canvasSizeWidthEdit     :: Editor T.Text Name
             , _canvasSizeHeightEdit    :: Editor T.Text Name
             , _canvasSizeFocus         :: FocusRing Name
             , _canvasOffset            :: Location
             , _canvasPath              :: Maybe FilePath
             , _canvasDirty             :: Bool
             , _askToSaveFilenameEdit   :: Editor T.Text Name
             , _appEventChannel         :: BChan AppEvent
             , _textEntered             :: T.Text
             , _textEntryStart          :: (Int, Int)
             , _boxStyleIndex           :: Int
             }

makeLenses ''AppState
