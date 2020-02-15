{-# LANGUAGE GADTs #-}
module Tart.Format.Types
  ( TartFile(..)
  , TartFileFormat(..)
  )
where

import qualified Data.Binary as B
import qualified Data.Text as T

import Tart.Canvas

data TartFile =
    TartFile { tartFileCanvasList  :: [Canvas]
             , tartFileCanvasNames :: [T.Text]
             , tartFileCanvasOrder :: [Int]
             }

data TartFileFormat where
    BinaryFormatVersion :: (B.Get a)
                        -> (a -> IO (Either String TartFile))
                        -> TartFileFormat
