module Main where

import Control.Monad (void)
import Brick
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Monoid ((<>))

import App
import Util
import Canvas

main :: IO ()
main = do
    checkForMouseSupport

    args <- getArgs
    c <- case args of
        [f] -> do
            r <- readCanvas f
            case r of
                Left e -> do
                    putStrLn $ f <> ": could not read file: " <> e
                    exitFailure
                Right c -> return $ Just (f, c)
        _ -> return Nothing

    (void . defaultMain application) =<< mkInitialState c
