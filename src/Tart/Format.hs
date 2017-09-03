module Tart.Format
  ( TartFile(..)
  , OutputFormat(..)
  , TartFilePath
  , readTartFile
  , writeTartFile
  , sortedCanvases
  , toTartFilepath
  )
where

import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import qualified Data.Binary.Put as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Tart.Canvas
import Tart.Format.Types
import Tart.Format.V0
import Tart.Format.V1
import Tart.Format.V2

data OutputFormat =
    FormatBinary
    | FormatAnsiColor
    | FormatPlain
    deriving (Eq, Show, Read)

formats :: [TartFileFormat]
formats =
    [ version2Format
    , version1Format
    , version0Format
    ]

newtype TartFilePath = TartFilePath FilePath

tartFilenameExtension :: String
tartFilenameExtension = ".tart"

toTartFilepath :: FilePath -> TartFilePath
toTartFilepath p =
    if tartFilenameExtension `isSuffixOf` p
    then TartFilePath $ take (length p - length tartFilenameExtension) p
    else TartFilePath p

readTartFile :: TartFilePath -> IO (Either String TartFile)
readTartFile (TartFilePath path) = do
    bs <- BS.readFile $ path <> tartFilenameExtension
    readTartFile' (BSL.fromStrict bs) path formats

readTartFile' :: BSL.ByteString -> FilePath -> [TartFileFormat] -> IO (Either String TartFile)
readTartFile' _ path [] = return $ Left $ path <> ": could not load file"
readTartFile' bs path ((BinaryFormatVersion parser converter):fmts) = do
    let tryNextFormat = readTartFile' bs path fmts
    case B.runGetOrFail parser bs of
        Left _ -> tryNextFormat
        Right (remaining, _, d) ->
            case BSL.null remaining of
                False -> tryNextFormat
                True -> do
                    result <- converter d
                    case result of
                        Left _ -> tryNextFormat
                        Right tf -> return $ Right tf

writeTartFile :: OutputFormat -> TartFile -> TartFilePath -> IO ()
writeTartFile format =
    case format of
          FormatPlain     -> writeTartFilePretty False
          FormatAnsiColor -> writeTartFilePretty True
          FormatBinary    -> writeTartFileBinary

sortedCanvases :: [Int] -> [Canvas] -> [Canvas]
sortedCanvases order cs =
    [ cs !! i | i <- order ]

tartFileCanvasesSorted :: TartFile -> [Canvas]
tartFileCanvasesSorted tf =
    sortedCanvases (tartFileCanvasOrder tf)
                   (tartFileCanvasList tf)

writeTartFilePretty :: Bool -> TartFile -> TartFilePath -> IO ()
writeTartFilePretty color tf (TartFilePath path) =
    let ext = if color then ".color.txt"
                       else ".plain.txt"
        fn = path <> ext
    in writeFile fn $
       prettyPrintCanvas color $ tartFileCanvasesSorted tf

writeTartFileBinary :: TartFile -> TartFilePath -> IO ()
writeTartFileBinary tf (TartFilePath path) =
    let fn = path <> tartFilenameExtension
    in BS.writeFile fn $ BSL.toStrict $ B.runPut $ latestVersionEncoder tf

latestVersionEncoder :: TartFile -> B.Put
latestVersionEncoder = encodeVersion2
