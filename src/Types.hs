{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord
  , Pixel
  , Tool(..)

  , AppState(..)
  , drawing
  , canvasSize
  , mode
  , tool
  , showHud
  , drawPaletteIndex
  , palette

  , blankPixel
  )
where

import Lens.Micro.TH
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

data Mode = Main
          deriving (Eq, Show)

data Name = Canvas
          | PaletteEntry Int
          deriving (Eq, Show, Ord)

data Tool = FreeHand
          deriving (Eq, Show)

type Coord = (Int, Int)

type Pixel = (Char, V.Attr)

blankPixel :: Pixel
blankPixel = (' ', V.defAttr)

data AppState =
    AppState { _drawing          :: Vec.Vector (Vec.Vector Pixel)
             , _canvasSize       :: (Int, Int)
             , _mode             :: Mode
             , _drawPaletteIndex :: Int
             , _tool             :: Tool
             , _showHud          :: Bool
             , _palette          :: Vec.Vector V.Color
             }

makeLenses ''AppState
