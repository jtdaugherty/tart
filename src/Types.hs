{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord
  , Pixel
  , Tool(..)
  , PaletteEntry(..)

  , AppState(..)
  , drawing
  , drawingFrozen
  , canvasSize
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
  , dragging
  , canvasSizeWidthEdit
  , canvasSizeHeightEdit
  , canvasSizeFocus
  , canvasOffset
  )
where

import Brick (Extent, Location)
import Brick.Focus
import Brick.Widgets.Edit (Editor)
import qualified Data.Text as T
import Data.Word (Word64)
import Lens.Micro.TH
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Data.Array.IO (IOUArray)
import Data.Array.Unboxed (UArray)

data Mode = Main
          | CharacterSelect
          | FgPaletteEntrySelect
          | BgPaletteEntrySelect
          | ToolSelect
          | CanvasSizePrompt
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
          deriving (Eq, Show, Ord)

data Tool = FreeHand
          | Eraser
          deriving (Eq, Show, Ord)

type Coord = (Int, Int)

type Pixel = (Char, V.Attr)

data PaletteEntry =
    PaletteEntry { paletteFg :: V.Attr -> V.Attr
                 , paletteBg :: V.Attr -> V.Attr
                 }

data AppState =
    AppState { _drawing                 :: IOUArray (Int, Int) Word64
             , _drawingFrozen           :: UArray (Int, Int) Word64
             , _canvasSize              :: (Int, Int)
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
             , _dragging                :: Maybe Name
             , _canvasSizeWidthEdit     :: Editor T.Text Name
             , _canvasSizeHeightEdit    :: Editor T.Text Name
             , _canvasSizeFocus         :: FocusRing Name
             , _canvasOffset            :: Location
             }

makeLenses ''AppState
