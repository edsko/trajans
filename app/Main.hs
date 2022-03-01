module Main (main) where

import Data.List (intersperse)

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import Trajans.Grid
import Trajans.Letter
import Trajans.Trajan
import Trajans.Util.Diagrams

main :: IO ()
main = mainWith $ growBoundingBox (r2 (2, 1)) $ vcat $ intersperse (strutY 1) [
      hcat $ intersperse (strutX 1) $ map renderLetter "ABCDE"
    , hcat $ intersperse (strutX 1) $ map renderLetter "FGHJK"
    , hcat $ intersperse (strutX 1) $ map renderLetter "LMNOP"
    , hcat $ intersperse (strutX 1) $ map renderLetter "QRSTU"
    , hcat $ intersperse (strutX 1) $ map renderLetter "VWXY"
    , hcat $ intersperse (strutX 1) $ map renderLetter "Z"
    ]

renderLetter :: Char -> Diagram B
renderLetter c = (strokeLetter (trajan c) # lc blue # lw medium) <> grid

