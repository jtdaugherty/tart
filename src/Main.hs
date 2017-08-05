module Main where

import Control.Monad (void)
import Brick

import App
import Util

main :: IO ()
main = do
    checkForMouseSupport
    st <- mkInitialState
    void $ defaultMain application st
