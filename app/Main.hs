module Main (main) where

import Data.Char (toUpper)
import Data.Foldable (asum)
import Data.List (intersperse)
import Options.Applicative

import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import Trajans.Grid
import Trajans.Letter
import Trajans.Text
import Trajans.Trajan
import Trajans.Util.Diagrams
import Data.Maybe (mapMaybe, fromMaybe)

main :: IO ()
main = mainWith (growBoundingBox (r2 (1, 1)) . execRenderCommand)

execRenderCommand :: RenderCommand -> Diagram B
execRenderCommand RenderAlphabet   = renderAlphabet
execRenderCommand (RenderText a t) = renderText a t

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data RenderCommand =
    RenderAlphabet
  | RenderText (Maybe Alignment) String
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
          <*> optional parseAlignment
          <*> ( strOption $ mconcat [
                    long "text"
                  , help "Text to render"
                  ])
      ]

parseAlignment :: Parser Alignment
parseAlignment = asum [
      flag' AlignLeft $ mconcat [
          long "left"
        , help "Align left"
        ]
    , flag' AlignCenter $ mconcat [
          long "center"
        , help "Align centered"
        ]
    , flag' AlignRight $ mconcat [
          long "right"
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
  where
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

{-------------------------------------------------------------------------------
  Rendering text
-------------------------------------------------------------------------------}

renderText :: Maybe Alignment -> String -> Diagram B
renderText ma =
      mconcat
    . map (uncurry translateY)
    . zip [0, 0 - interLineSpacing ..]
    . map (renderLine (fromMaybe AlignLeft ma))
    . mapMaybe constructLine
    . lines
    . map toUpper
  where
    interLineSpacing :: Double
    interLineSpacing = 13
