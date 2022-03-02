module Trajans.Grid (gridOfWidth) where

import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (width)

import Trajans.Util.Diagrams

gridOfWidth :: Int -> Bool -> Diagram B
gridOfWidth width renderCircle = mconcat [
      strokePath (gridPath width) # lw veryThin
    , if renderCircle
        then strokePath gridCircle # lw veryThin
        else mempty
    ]

gridPath :: Int -> Path V2 Double
gridPath width = gridLines width

gridCircle :: Path V2 Double
gridCircle = circle 5 # translate (r2 (5, 5))

gridLines :: Int -> Path V2 Double
gridLines width = Path $ map mkLocTrail (horizontal ++ vertical)
  where
    horizontal, vertical :: [[Point V2 Double]]
    horizontal = [[p2 (0, d i), p2 (d width, d i)] | i <- [0 .. 10   ]]
    vertical   = [[p2 (d i, 0), p2 (d i    , 10 )] | i <- [0 .. width]]

    d :: Int -> Double
    d = fromIntegral
