module Main where

import Brick
import Lens.Micro.Platform
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Monoid ((<>))

import App
import Util
import Canvas
import Types

main :: IO ()
main = do
    args <- getArgs
    c <- case args of
        [f] -> do
            r <- readCanvas f
            case r of
                Left e -> do
                    putStrLn $ f <> ": could not read file: " <> e
                    exitFailure
                Right c -> return $ Just c
        _ -> return Nothing

    checkForMouseSupport
    finalSt <- (defaultMain application) =<< mkInitialState c
    writeCanvasFriendly "out.txt" $ finalSt^.drawing
    writeCanvas "out.bin" $ finalSt^.drawing
