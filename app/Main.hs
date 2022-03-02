module Main (main) where

import Data.Foldable (asum)
import Data.List (intersperse)
import Options.Applicative

import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import Trajans.Grid
import Trajans.Letter
import Trajans.Trajan
import Trajans.Util.Diagrams

main :: IO ()
main = mainWith (growBoundingBox (r2 (1, 1)) . execRenderCommand)

execRenderCommand :: RenderCommand -> Diagram B
execRenderCommand RenderAlphabet = renderAlphabet

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data RenderCommand =
    RenderAlphabet
  | RenderText String (Maybe Alignment)
  deriving (Show)

data Alignment =
    AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Show)

instance Parseable RenderCommand where
  parser = asum [
        flag' RenderAlphabet $ mconcat [
             long "alphabet"
           , help "Render the full alphabet"
           ]
      , const RenderText
          <$> ( flag' () $ mconcat [
                    long "render"
                  , help "Render text"
                  ])
          <*> ( strOption $ mconcat [
                    long "text"
                  , help "Text to render"
                  ])
          <*> optional parseAlignment
      ]

parseAlignment :: Parser Alignment
parseAlignment = asum [
      flag' AlignLeft $ mconcat [
          long "left"
        , help "Align left"
        ]
    , flag' AlignLeft $ mconcat [
          long "center"
        , help "Align centered"
        ]
    , flag' AlignLeft $ mconcat [
          long "left"
        , help "Align right"
        ]
    ]

{-------------------------------------------------------------------------------
  Render the full alphabet
-------------------------------------------------------------------------------}

renderAlphabet :: Diagram B
renderAlphabet = vcat $ intersperse (strutY 1) [
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
