module Main where

import Control.Monad (void, when)
import Control.Applicative ((<|>))
import Brick
import Brick.BChan (newBChan)
import qualified Graphics.Vty as V
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Console.GetOpt
import Data.Monoid ((<>))
import System.Directory (doesFileExist)

import App
import State
import Tart.Canvas
import Tart.Format

data Option = Import FilePath
            | Output FilePath
            | ShowHelp
            deriving (Eq)

data Config =
    Config { configImport :: Maybe FilePath
           , configOutput :: Maybe FilePath
           , configShowHelp :: Bool
           }

defaultConfig :: Config
defaultConfig =
    Config { configImport = Nothing
           , configOutput = Nothing
           , configShowHelp = False
           }

opts :: [OptDescr Option]
opts =
    [ Option "i" ["import"] (ReqArg Import "FILE")
      "Import a plain text file to begin a drawing"
    , Option "o" ["output"] (ReqArg Output "FILE")
      "Where to write the output file"
    , Option "h" ["help"] (NoArg ShowHelp)
      "Show this help"
    ]

configFromOpts :: [Option] -> Config -> IO Config
configFromOpts [] c =
    return c
configFromOpts (ShowHelp:os) c =
    configFromOpts os $ c { configShowHelp = True }
configFromOpts (Output f:os) c = do
    configFromOpts os $ c { configOutput = Just f }
configFromOpts (Import f:os) c = do
    ex <- doesFileExist f
    case ex of
        False -> error $ "file not found: " <> f
        True -> configFromOpts os $ c { configImport = Just f }

showHelp :: IO ()
showHelp = do
    pn <- getProgName
    putStrLn $ usageInfo ("Usage: " <> pn <> " <options> [file]") opts

main :: IO ()
main = do
    checkForMouseSupport

    args <- getArgs
    let (os, rest, errs) = getOpt Permute opts args

    when (not $ null errs) $
        showHelp >> exitFailure

    cfg <- configFromOpts os defaultConfig

    when (configShowHelp cfg) $
        showHelp >> exitFailure

    -- If this is an import operation, read the plain text file and
    -- convert to a canvas. If an output filename was specified, write
    -- the file and exit. Otherwise look for an input file in canvas
    -- format
    c <- case configImport cfg of
        Just f -> do
            s <- readFile f
            c <- canvasFromText s
            case configOutput cfg of
                Nothing -> return $ Just (Nothing, [c], [0], ["default"])
                Just output -> do
                    writeCanvasFiles output [c] [0] ["default"]
                    exitSuccess
        Nothing ->
            case rest of
                [f] -> do
                    r <- readTartFile $ toTartFilepath f
                    case r of
                        Left e -> do
                            putStrLn $ f <> ": could not read file: " <> e
                            exitFailure
                        Right tf ->
                            return $ Just ( configOutput cfg <|> Just f
                                          , tartFileCanvasList tf
                                          , tartFileCanvasOrder tf
                                          , tartFileCanvasNames tf
                                          )
                _ -> return Nothing

    chan <- newBChan 10
    let mkVty = V.mkVty =<< V.standardIOConfig

    (void . customMain mkVty (Just chan) application) =<< mkInitialState chan c
