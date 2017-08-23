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
    TartFile { tartFileCanvasList  :: [Canvas]
             , tartFileCanvasNames :: [String]
             , tartFileCanvasOrder :: [Int]
             }

data TartFileData =
    TartFileData { tartFileDataCanvasData  :: [CanvasData]
                 , tartFileDataCanvasNames :: [String]
                 , tartFileDataCanvasOrder :: [Int]
                 }

data OutputFormat =
    FormatBinary
    | FormatAnsiColor
    | FormatPlain
    deriving (Eq, Show, Read)

instance B.Binary TartFileData where
    put d = do
        B.put $ tartFileDataCanvasData d
        B.put $ tartFileDataCanvasNames d
        B.put $ tartFileDataCanvasOrder d
    get =
        TartFileData <$> B.get
                     <*> B.get
                     <*> B.get

tartFileToData :: TartFile -> TartFileData
tartFileToData tf =
    TartFileData (canvasToData <$> tartFileCanvasList tf)
                 (tartFileCanvasNames tf)
                 (tartFileCanvasOrder tf)

tartFileFromData :: TartFileData -> IO (Either String TartFile)
tartFileFromData d = do
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

    result <- loadCanvases (tartFileDataCanvasData d)
    case result of
        Left s -> return $ Left s
        Right cs -> return $ Right $ TartFile cs (tartFileDataCanvasNames d)
                                                 (tartFileDataCanvasOrder d)

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
          FormatPlain     -> writeCanvasPretty False
          FormatAnsiColor -> writeCanvasPretty True
          FormatBinary    -> writeCanvasBinary

tartFileCanvasesSorted :: TartFile -> [Canvas]
tartFileCanvasesSorted tf =
    let cs = tartFileCanvasList tf
    in [ cs !! i | i <- tartFileCanvasOrder tf ]

writeCanvasPretty :: Bool -> TartFile -> FilePath -> IO ()
writeCanvasPretty color tf path =
    writeFile path $ prettyPrintCanvas color $ tartFileCanvasesSorted tf

writeCanvasBinary :: TartFile -> FilePath -> IO ()
writeCanvasBinary tf path =
    BS.writeFile path $ BSL.toStrict $ B.runPut $ B.put (tartFileToData tf)
