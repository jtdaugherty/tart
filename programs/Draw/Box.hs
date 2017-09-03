{-# LANGUAGE TupleSections #-}
module Draw.Box
  ( plotBox
  )
where

import Data.Monoid ((<>))
import Lens.Micro.Platform
import Brick.Widgets.Border.Style

plotBox :: BorderStyle -> (Int, Int) -> (Int, Int) -> [((Int, Int), Char)]
plotBox bs a b =
    let (ul, lr) = boxCorners a b
        (ll, ur) = ( (ul^._1, lr^._2)
                   , (lr^._1, ul^._2)
                   )
        top =    (, bsHorizontal bs) <$> (, ul^._2) <$> [ul^._1 + 1..ur^._1 - 1]
        bottom = (, bsHorizontal bs) <$> (, ll^._2) <$> [ll^._1 + 1..lr^._1 - 1]
        left =   (, bsVertical bs)   <$> (ul^._1, ) <$> [ul^._2 + 1..ll^._2 - 1]
        right =  (, bsVertical bs)   <$> (ur^._1, ) <$> [ur^._2 + 1..lr^._2 - 1]

        width = lr^._1 - ul^._1
        height = lr^._2 - ul^._2
        corners = if width == 0
                  then [ (ul, bsVertical bs)
                       , (lr, bsVertical bs)
                       ]
                  else if height == 0
                       then [ (ul, bsHorizontal bs)
                            , (lr, bsHorizontal bs)
                            ]
                       else [ (ul, bsCornerTL bs)
                            , (lr, bsCornerBR bs)
                            , (ll, bsCornerBL bs)
                            , (ur, bsCornerTR bs)
                            ]

        -- Draw the corners
        pixels = corners <>
                 -- Draw the top and bottom
                 top <>
                 bottom <>
                 -- Draw the sides
                 left <>
                 right
    in pixels

boxCorners :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
boxCorners (a0, a1) (b0, b1) =
    let ul = (min a0 b0, min a1 b1)
        lr = (max a0 b0, max a1 b1)
    in (ul, lr)
