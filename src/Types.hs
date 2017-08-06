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
  , tool
  , showHud

  , blankCharacter
  )
where

import Lens.Micro.TH
import qualified Data.Vector as V

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
    AppState { _drawing       :: V.Vector (V.Vector Char)
             , _canvasSize    :: (Int, Int)
             , _mode          :: Mode
             , _tool          :: Tool
             , _showHud       :: Bool
             }

makeLenses ''AppState
