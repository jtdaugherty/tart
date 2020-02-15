module Tart.Format.V0
  ( version0Format
  )
where

import qualified Data.Binary as B
import qualified Data.Text as T

import Tart.Canvas
import Tart.Format.Types

data TartFileDataV0 =
    TartFileDataV0 { tartFileDataV0CanvasData  :: CanvasData
                   }

version0Format :: TartFileFormat
version0Format =
    BinaryFormatVersion B.get tartFileFromDataV0

instance B.Binary TartFileDataV0 where
    put d =
        B.put $ tartFileDataV0CanvasData d
    get =
        TartFileDataV0 <$> B.get

tartFileFromDataV0 :: TartFileDataV0 -> IO (Either String TartFile)
tartFileFromDataV0 d = do
    result <- canvasFromData (tartFileDataV0CanvasData d)
    case result of
        Left s -> return $ Left s
        Right c -> return $ Right $ TartFile [c] [T.pack "default"] [0]
