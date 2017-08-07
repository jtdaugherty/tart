module Main where

import Control.Monad (void)
import Brick

import App
import Util

main :: IO ()
main = do
    checkForMouseSupport
    (void . defaultMain application) =<< mkInitialState
