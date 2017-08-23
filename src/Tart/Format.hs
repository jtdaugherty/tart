module Tart.Format
  ( TartFile(..)
  , OutputFormat(..)
  , readTartFile
  , writeTartFile
  )
where

import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Tart.Canvas

data TartFile =
    TartFile { tartFileCanvas :: Canvas
             }

data TartFileData =
    TartFileData { tartFileCanvasData :: CanvasData
                 }

data OutputFormat =
    Binary
    | Color
    | Plain
    deriving (Eq, Show, Read)

instance B.Binary TartFileData where
    put d = B.put $ tartFileCanvasData d
    get = TartFileData <$> B.get

tartFileToData :: TartFile -> TartFileData
tartFileToData tf = TartFileData $ canvasToData $ tartFileCanvas tf

tartFileFromData :: TartFileData -> IO (Either String TartFile)
tartFileFromData d = do
    result <- canvasFromData $ tartFileCanvasData d
    case result of
        Left s -> return $ Left s
        Right c -> return $ Right $ TartFile c

readTartFile :: FilePath -> IO (Either String TartFile)
readTartFile path = do
    bs <- BS.readFile path
    case B.runGetOrFail B.get (BSL.fromStrict bs) of
        Left (_, _, s) -> return $ Left s
        Right (remaining, _, d) ->
            case BSL.null remaining of
                False -> return $ Left "File contained unused bytes"
                True -> do
                    result <- tartFileFromData d
                    case result of
                        Left msg -> return $ Left msg
                        Right tf -> return $ Right tf

writeTartFile :: OutputFormat -> TartFile -> FilePath -> IO ()
writeTartFile format =
    case format of
          Plain -> writeCanvasPretty False
          Color -> writeCanvasPretty True
          Binary -> writeCanvasBinary

writeCanvasPretty :: Bool -> TartFile -> FilePath -> IO ()
writeCanvasPretty color tf path =
    writeFile path $ prettyPrintCanvas color [tartFileCanvas tf]

writeCanvasBinary :: TartFile -> FilePath -> IO ()
writeCanvasBinary tf path =
    BS.writeFile path $ BSL.toStrict $ B.runPut $ B.put (tartFileToData tf)
