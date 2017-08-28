module Tart.Format.V2
  ( version2Format
  , encodeVersion2
  )
where

import Control.Monad (when)
import Data.Int (Int32)
import qualified Data.Binary as B

import Tart.Canvas
import Tart.Format.Types

data TartFileDataV2 =
    TartFileDataV2 { tartFileDataV2CanvasData  :: [CanvasData]
                   , tartFileDataV2CanvasNames :: [String]
                   , tartFileDataV2CanvasOrder :: [Int]
                   }

tartFileDataV2Magic :: Int32
tartFileDataV2Magic = 0xcafe02

encodeVersion2 :: TartFile -> B.Put
encodeVersion2 = B.put . tartFileToDataV2

version2Format :: TartFileFormat
version2Format =
    BinaryFormatVersion B.get tartFileFromDataV2

instance B.Binary TartFileDataV2 where
    put d = do
        B.put tartFileDataV2Magic
        B.put $ tartFileDataV2CanvasData d
        B.put $ tartFileDataV2CanvasNames d
        B.put $ tartFileDataV2CanvasOrder d
    get = do
        magic <- B.get
        when (magic /= tartFileDataV2Magic) $
            fail "not a valid tart file version 1"

        TartFileDataV2 <$> B.get
                       <*> B.get
                       <*> B.get

tartFileToDataV2 :: TartFile -> TartFileDataV2
tartFileToDataV2 tf =
    TartFileDataV2 (canvasToData <$> tartFileCanvasList tf)
                   (tartFileCanvasNames tf)
                   (tartFileCanvasOrder tf)

tartFileFromDataV2 :: TartFileDataV2 -> IO (Either String TartFile)
tartFileFromDataV2 d = do
    let loadCanvases [] = return $ Right []
        loadCanvases (cd:cds) = do
            result <- canvasFromData cd
            case result of
                Left e -> return $ Left e
                Right c -> do
                    rest <- loadCanvases cds
                    case rest of
                        Left e -> return $ Left e
                        Right cs -> return $ Right $ c : cs

    result <- loadCanvases (tartFileDataV2CanvasData d)
    case result of
        Left s -> return $ Left s
        Right cs -> return $ Right $ TartFile cs (tartFileDataV2CanvasNames d)
                                                 (tartFileDataV2CanvasOrder d)
