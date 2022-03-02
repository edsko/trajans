module Main (main) where

import Data.List (intersperse)

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import Trajans.Grid
import Trajans.Letter
import Trajans.Trajan
import Trajans.Util.Diagrams

main :: IO ()
main = mainWith $ growBoundingBox (r2 (1, 1)) $ vcat $ intersperse (strutY 1) [
      hcat $ intersperse (strutX 1) $ map renderLetter "ABCDE"
    , hcat $ intersperse (strutX 1) $ map renderLetter "FGHIJ"
    , hcat $ intersperse (strutX 1) $ map renderLetter "KLMNO"
    , hcat $ intersperse (strutX 1) $ map renderLetter "PQRST"
    , hcat $ intersperse (strutX 1) $ map renderLetter "UVWXY"
    , hcat $ intersperse (strutX 1) $ map renderLetter "Z"
    ]

renderLetter :: Char -> Diagram B
renderLetter c = mconcat [
      strokeLetter l # lc blue # lw medium
    , fromVertices [ p2 (letterOffset + fst letterBounds, -1)
                   , p2 (letterOffset + fst letterBounds, 11)
                   ] # lc red
    , fromVertices [ p2 (letterOffset + snd letterBounds, -1)
                   , p2 (letterOffset + snd letterBounds, 11)
                   ] # lc red
    , grid
    ]
  where
   l@Letter{..} = trajan c
