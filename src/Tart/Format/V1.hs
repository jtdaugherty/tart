module Tart.Format.V1
  ( version1Format
  , encodeVersion1
  )
where

import qualified Data.Binary as B
import qualified Data.Text as T

import Tart.Canvas
import Tart.Format.Types

data TartFileDataV1 =
    TartFileDataV1 { tartFileDataV1CanvasData  :: [CanvasData]
                   , tartFileDataV1CanvasNames :: [T.Text]
                   , tartFileDataV1CanvasOrder :: [Int]
                   }

encodeVersion1 :: TartFile -> B.Put
encodeVersion1 = B.put . tartFileToDataV1

version1Format :: TartFileFormat
version1Format =
    BinaryFormatVersion B.get tartFileFromDataV1

instance B.Binary TartFileDataV1 where
    put d = do
        B.put $ tartFileDataV1CanvasData d
        B.put $ T.unpack <$> tartFileDataV1CanvasNames d
        B.put $ tartFileDataV1CanvasOrder d
    get = do
        TartFileDataV1 <$> B.get
                       <*> (fmap T.pack <$> B.get)
                       <*> B.get

tartFileToDataV1 :: TartFile -> TartFileDataV1
tartFileToDataV1 tf =
    TartFileDataV1 (canvasToData <$> tartFileCanvasList tf)
                   (tartFileCanvasNames tf)
                   (tartFileCanvasOrder tf)

tartFileFromDataV1 :: TartFileDataV1 -> IO (Either String TartFile)
tartFileFromDataV1 d = do
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

    result <- loadCanvases (tartFileDataV1CanvasData d)
    case result of
        Left s -> return $ Left s
        Right cs -> return $ Right $ TartFile cs (tartFileDataV1CanvasNames d)
                                                 (tartFileDataV1CanvasOrder d)
