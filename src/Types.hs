{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord
  , Pixel
  , Tool(..)
  , PaletteEntry(..)
  , AppEvent(..)

  , AppState(..)
  , drawing
  , mode
  , tool
  , showHud
  , drawFgPaletteIndex
  , drawBgPaletteIndex
  , palette
  , drawCharacter
  , fgPaletteSelectorExtent
  , bgPaletteSelectorExtent
  , toolSelectorExtent
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

import Canvas

data AppEvent =
    DragFinished Name Location Location
    deriving (Eq)

data Mode = Main
          | CharacterSelect
          | FgPaletteEntrySelect
          | BgPaletteEntrySelect
          | ToolSelect
          | CanvasSizePrompt
          | AskToSave
          deriving (Eq, Show)

data Name = Canvas
          | Hud
          | ToolSelector
          | ToolSelectorEntry Tool
          | CharSelector
          | FgSelector
          | BgSelector
          | FgPaletteEntry Int
          | BgPaletteEntry Int
          | ResizeCanvas
          | CanvasSizeWidthEdit
          | CanvasSizeHeightEdit
          | AskToSaveFilenameEdit
          deriving (Eq, Show, Ord)

data Tool = Freehand
          | Recolor
          | Eraser
          deriving (Eq, Show, Ord)

type Coord = (Int, Int)

type Pixel = (Char, V.Attr)

data PaletteEntry =
    PaletteEntry { paletteFg :: V.Attr -> V.Attr
                 , paletteBg :: V.Attr -> V.Attr
                 }

data AppState =
    AppState { _drawing                 :: Canvas
             , _mode                    :: Mode
             , _drawFgPaletteIndex      :: Int
             , _drawBgPaletteIndex      :: Int
             , _drawCharacter           :: Char
             , _tool                    :: Tool
             , _showHud                 :: Bool
             , _palette                 :: Vec.Vector PaletteEntry
             , _fgPaletteSelectorExtent :: Maybe (Extent Name)
             , _bgPaletteSelectorExtent :: Maybe (Extent Name)
             , _toolSelectorExtent      :: Maybe (Extent Name)
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
             }

makeLenses ''AppState
