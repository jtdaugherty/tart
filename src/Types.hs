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

  , blankPixel
  )
where

import Brick (Extent)
import Lens.Micro.TH
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

data Mode = Main
          | CharacterSelect
          | FgPaletteEntrySelect
          | BgPaletteEntrySelect
          | ToolSelect
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

blankPixel :: Pixel
blankPixel = (' ', V.defAttr)

data AppState =
    AppState { _drawing                 :: Vec.Vector (Vec.Vector Pixel)
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
             }

makeLenses ''AppState
