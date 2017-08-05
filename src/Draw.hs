module Draw
  ( drawAtPoint
  )
where

import Brick
import Control.Monad.Trans (liftIO)
import qualified Data.Array.MArray as A
import Lens.Micro.Platform

import Types

drawAtPoint :: AppState -> (Int, Int) -> EventM Name AppState
drawAtPoint s point = refreeze $ do
    let arr = s^.drawing
    A.writeArray arr point 'x'
    return s

refreeze :: IO AppState -> EventM Name AppState
refreeze mkS = do
    liftIO $ do
        s <- mkS
        f <- A.freeze $ s^.drawing
        return $ s & drawingFrozen .~ f
