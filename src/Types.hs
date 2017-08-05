{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord
  , Tool(..)

  , AppState(..)
  , drawing
  , canvasSize
  , mode
  , drawingFrozen
  , tool
  , showHud

  , blankCharacter
  )
where

import Data.Array.IO (IOUArray)
import Data.Array.Unboxed (UArray)
import Lens.Micro.TH

data Mode = Main
          deriving (Eq, Show)

data Name = Canvas
          deriving (Eq, Show, Ord)

data Tool = Point
          deriving (Eq, Show)

type Coord = (Int, Int)

blankCharacter :: Char
blankCharacter = ' '

data AppState =
    AppState { _drawing       :: IOUArray Coord Char
             , _drawingFrozen :: UArray Coord Char
             , _canvasSize    :: (Int, Int)
             , _mode          :: Mode
             , _tool          :: Tool
             , _showHud       :: Bool
             }

makeLenses ''AppState
