{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord
  , Pixel
  , Tool(..)
  , AppEvent(..)
  , isBox
  , getToolBorderStyle
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
  )
where

import Brick (Extent, Location)
import Brick.BChan (BChan)
import Brick.Focus
import Brick.Widgets.Edit (Editor)
import Brick.Widgets.Border.Style
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
          | TextEntry
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
          | TextEntryCursor
          deriving (Eq, Show, Ord)

data Tool = Freehand
          | BoxAscii
          | BoxUnicode
          | BoxRounded
          | Recolor
          | Eyedropper
          | FloodFill
          | Eraser
          | TextString
          deriving (Eq, Show, Ord)

toolName :: Tool -> String
toolName Freehand   = "Freehand"
toolName BoxAscii   = "Box (ASCII)"
toolName BoxUnicode = "Box (Unicode)"
toolName BoxRounded = "box (Rounded)"
toolName Recolor    = "Re-color"
toolName Eraser     = "Eraser"
toolName Eyedropper = "Eyedropper"
toolName FloodFill  = "Flood fill"
toolName TextString = "Text string"

isBox :: Tool -> Bool
isBox = (`elem` [BoxAscii, BoxUnicode, BoxRounded])

getToolBorderStyle :: Tool -> BorderStyle
getToolBorderStyle BoxAscii = ascii
getToolBorderStyle BoxUnicode = unicode
getToolBorderStyle BoxRounded = unicodeRounded
getToolBorderStyle _ = ascii

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
             }

makeLenses ''AppState
