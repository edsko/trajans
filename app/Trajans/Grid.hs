module Trajans.Grid (grid) where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import Trajans.Util.Diagrams

grid :: Diagram B
grid = strokePath gridPath # lw veryThin

gridPath :: Path V2 Double
gridPath = gridLines <> gridCircle

gridCircle :: Path V2 Double
gridCircle = circle 5 # translate (r2 (5, 5))

gridLines :: Path V2 Double
gridLines = Path $ map mkLocTrail (horizontal ++ vertical)
  where
    horizontal, vertical :: [[Point V2 Double]]
    horizontal = [[p2 (0, i), p2 (10, i)] | i <- [0 .. 10]]
    vertical   = [[p2 (i, 0), p2 (i, 10)] | i <- [0 .. 10]]
