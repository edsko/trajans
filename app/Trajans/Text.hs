module Trajans.Text (
    Line(..)
  , Space(..)
  , constructLine
  , renderLine
  ) where

import Prelude hiding (Word)

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import Diagrams.Prelude hiding (Line)
import Diagrams.Backend.SVG

import Trajans.Letter
import Trajans.Trajan
import Trajans.Util.Alternate (Alternate)
import Trajans.Util.Alternate qualified as Alt

{-------------------------------------------------------------------------------
  Line of text
-------------------------------------------------------------------------------}

newtype Line = Line (Alternate Letter Space)
  deriving (Show)

data Space =
    InterLetter
  | InterWord
  deriving (Show)

spacing :: Space -> Double
spacing InterLetter = 5
spacing InterWord   = 9

constructLine :: String -> Maybe Line
constructLine = fmap (Line . onWords) . parseLine
  where
    onWords :: NonEmpty Word -> Alternate Letter Space
    onWords = Alt.concat (\_ _ -> InterWord) . fmap onWord

    onWord :: Word -> Alternate Letter Space
    onWord (Word w) = Alt.fromList (\_ _ -> InterLetter) w

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

data WithOffset a = WithOffset a Double

renderWithOffset :: (a -> Diagram B) -> (WithOffset a) -> Diagram B
renderWithOffset f (WithOffset a x) = f a # translateX x

computeOffsets :: Line -> Alternate (WithOffset Letter) (WithOffset Space)
computeOffsets (Line l) =
    flip evalState initCursorPosition $
      Alt.traverse onLetter onSpace l
  where
    initCursorPosition :: Double
    initCursorPosition = 0

    onLetter :: Letter -> State Double (WithOffset Letter)
    onLetter = advanceBy letterWidth

    onSpace :: Letter -> Space -> Letter -> State Double (WithOffset Space)
    onSpace _ s _ = advanceBy spacing s

    advanceBy :: (a -> Double) -> a -> State Double (WithOffset a)
    advanceBy f x = state $ \cursor -> (x `WithOffset` cursor, cursor + f x)

renderLine :: Line -> Diagram B
renderLine =
      uncurry atop  -- We are careful to position the letter atop the spaces
    . bimap (foldMap (renderWithOffset renderLetter))
            (foldMap (renderWithOffset renderSpace))
    . Alt.partition
    . computeOffsets
  where
    renderLetter :: Letter -> Diagram B
    renderLetter l =
        strokeLetter l
          # translateX (-1 * (fst (letterBounds l) + letterOffset l))
          # showOrigin

    renderSpace :: Space -> Diagram B
    renderSpace s =
        (alignBL $ rect (spacing s) 10)
          # lw none
          # fc (spaceColor s)
          # showOrigin

    spaceColor :: Space -> Colour Double
    spaceColor InterLetter = pink
    spaceColor InterWord   = lightblue

{-------------------------------------------------------------------------------
  Sanitize user input
-------------------------------------------------------------------------------}

newtype Word = Word (NonEmpty Letter)

parseLine :: String -> Maybe (NonEmpty Word)
parseLine = (>>= NE.nonEmpty) . mapM parseWord . words

parseWord :: String -> Maybe Word
parseWord []     = Nothing
parseWord (x:xs) = Word <$> traverse aux (x :| xs)
  where
    aux :: Char -> Maybe Letter
    aux c | c >= 'A' && c <= 'Z' = Just $ trajan c
          | otherwise            = Nothing

