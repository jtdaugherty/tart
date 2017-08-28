{-# LANGUAGE GADTs #-}
module Tart.Format.Types
  ( TartFile(..)
  , TartFileFormat(..)
  )
where

import qualified Data.Binary as B

import Tart.Canvas

data TartFile =
    TartFile { tartFileCanvasList  :: [Canvas]
             , tartFileCanvasNames :: [String]
             , tartFileCanvasOrder :: [Int]
             }

data TartFileFormat where
    BinaryFormatVersion :: (B.Get a)
                        -> (a -> IO (Either String TartFile))
                        -> TartFileFormat
