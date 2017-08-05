{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord

  , AppState(..)
  , drawing
  , canvasSize

  , blankCharacter
  )
where

import Data.Array.IO (IOUArray)
import Lens.Micro.TH

data Mode = Main
          deriving (Eq, Show)

data Name = Unused
          deriving (Eq, Show, Ord)

type Coord = (Int, Int)

blankCharacter :: Char
blankCharacter = ' '

data AppState =
    AppState { _drawing      :: IOUArray Coord Char
             , _canvasSize   :: (Int, Int)
             }

makeLenses ''AppState
