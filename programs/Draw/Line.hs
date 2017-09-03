{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Draw.Line
  ( plotLine
  )
where

import Data.Bits (shiftR)
import Control.Monad.State.Lazy
import Lens.Micro.Platform

data LinePlot =
    LinePlot { _lpX :: Int
             , _lpY :: Int
             , _lpDx1 :: Int
             , _lpDy1 :: Int
             , _lpDx2 :: Int
             , _lpDy2 :: Int
             , _lpLongest :: Int
             , _lpShortest :: Int
             , _lpNumerator :: Int
             , _lpPixels :: [(Int, Int)]
             }

makeLenses ''LinePlot

-- From:
-- http://tech-algorithm.com/articles/drawing-line-using-bresenham-algorithm/
plotLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
plotLine p0 p1 = finalSt^.lpPixels
    where
        finalSt = execState plot (LinePlot 0 0 0 0 0 0 0 0 0 [])
        plot = do
            let ((x0, y0), (x1, y1)) = (p0, p1)
                w = x1 - x0
                h = y1 - y0

            lpX .= x0
            lpY .= y0
            lpDx1 .= 0
            lpDy1 .= 0
            lpDx2 .= 0
            lpDy2 .= 0

            if | w<0 -> lpDx1 .= -1
               | w>0 -> lpDx1 .= 1
               | otherwise -> return ()

            if | h<0 -> lpDy1 .= -1
               | h>0 -> lpDy1 .= 1
               | otherwise -> return ()

            if | w<0 -> lpDx2 .= -1
               | w>0 -> lpDx2 .= 1
               | otherwise -> return ()

            lpLongest  .= abs w
            lpShortest .= abs h

            longest <- use lpLongest
            shortest <- use lpShortest

            when (not $ longest > shortest) $ do
                lpLongest .= abs h
                lpShortest .= abs w

                if | (h<0) -> lpDy2 .= -1
                   | (h>0) -> lpDy2 .= 1
                   | otherwise -> return ()

                lpDx2 .= 0

            longest' <- use lpLongest
            lpNumerator .= (longest' `shiftR` 1)

            forM_ [0..longest'] $ \_ -> do
                x <- use lpX
                y <- use lpY
                lpPixels %= ((x, y):)

                shortest' <- use lpShortest
                lpNumerator += shortest'
                numerator <- use lpNumerator
                longest'' <- use lpLongest
                if not $ numerator < longest''
                   then do
                       lpNumerator -= longest''
                       (lpX +=) =<< use lpDx1
                       (lpY +=) =<< use lpDy1
                   else do
                       (lpX +=) =<< use lpDx2
                       (lpY +=) =<< use lpDy2
