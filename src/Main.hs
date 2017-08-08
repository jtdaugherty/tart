module Main where

import Brick
import Lens.Micro.Platform

import App
import Util
import Canvas
import Types

main :: IO ()
main = do
    checkForMouseSupport
    finalSt <- (defaultMain application) =<< mkInitialState
    writeCanvas "out.txt" $ finalSt^.drawing
