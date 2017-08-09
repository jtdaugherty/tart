module Main where

import Control.Monad (void)
import Brick
import Brick.BChan (newBChan)
import qualified Graphics.Vty as V
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

    chan <- newBChan 10
    let mkVty = V.mkVty =<< V.standardIOConfig

    (void . customMain mkVty (Just chan) application) =<< mkInitialState chan c
