module Util
  ( checkForMouseSupport
  , resizeCanvas
  , setTool
  , toggleHud
  )
where

import Control.Monad (when, forM_)
import Control.Monad.Trans (liftIO)
import qualified Graphics.Vty as V
import qualified Data.Array.MArray as A
import System.Exit (exitFailure)
import Lens.Micro.Platform

import Brick

import Types

setTool :: AppState -> Tool -> AppState
setTool s t = s & tool .~ t

toggleHud :: AppState -> AppState
toggleHud s = s & showHud %~ not

checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- V.mkVty =<< V.standardIOConfig

    when (not $ V.supportsMode (V.outputIface vty) V.Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure

    V.shutdown vty

resizeCanvas :: AppState -> EventM n AppState
resizeCanvas s = do
    vty <- getVtyHandle
    newSz <- liftIO $ V.displayBounds $ V.outputIface vty

    -- If the new bounds are different than the old, create a new array
    -- and copy.
    case newSz /= s^.canvasSize of
        False -> return s
        True -> liftIO $ do
            -- Create a new draw array
            let newBounds = ((0, 0), (newSz & each %~ pred))
            newDraw <- A.newArray newBounds blankCharacter

            -- Use the difference in size to determine the range of data
            -- to copy to the new canvas
            let (maxW, maxH) = ( min (newSz^._1) (s^.canvasSize._1)
                               , min (newSz^._2) (s^.canvasSize._2)
                               )

            forM_ [0..maxW-1] $ \w ->
                forM_ [0..maxH-1] $ \h ->
                    A.writeArray newDraw (w, h) =<<
                        A.readArray (s^.drawing) (w, h)

            newDrawFrozen <- A.freeze newDraw

            return $ s & drawing .~ newDraw
                       & drawingFrozen .~ newDrawFrozen
                       & canvasSize .~ newSz
