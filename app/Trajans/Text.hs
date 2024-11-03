module Trajans.Text (
    Line(..)
  , Space(..)
  , Alignment(..)
  , RenderOptions(..)
  , Error(..)
  , constructLine
  , renderLine
  , renderLetter
  ) where

import Prelude hiding (Word)

import Control.Monad.State
import Data.Char (toUpper)
import Data.List.NonEmpty (NonEmpty(..))

import Diagrams.Prelude hiding (Line)
import Diagrams.Backend.Cairo

import Trajans.Grid
import Trajans.Letter
import Trajans.RenderOptions
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

spacing :: RenderOptions -> Space -> Double
spacing RenderOptions{..} space =
    case (renderSpacing, renderXCompr, space) of
      (Proper _ , False , InterLetter) -> 5
      (Proper _ , True  , InterLetter) -> compression * 5
      (Regular  , _     , InterLetter) -> 1
      (_        , False , InterWord  ) -> 9
      (_        , True  , InterWord  ) -> compression * 9
  where
    compression :: Double
    compression = 3.5 / 5

constructLine :: String -> Either Error Line
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
  deriving (Show)

renderWithOffset :: (a -> Diagram B) -> (WithOffset a) -> Diagram B
renderWithOffset f (WithOffset a x) =
    f a
--      # showOrigin -- For debugging spacing
      # translateX x

-- | Compute offsets of all letters and spaces
--
-- Additionally returns the total width of the line (within the bounds)
computeOffsets ::
     RenderOptions
  -> Line
  -> (Alternate (WithOffset Letter) (WithOffset Space), Double)
computeOffsets opts@RenderOptions{..} (Line l) =
    flip runState initCursorPosition $
      Alt.traverse onLetter onSpace l
  where
    initCursorPosition :: Double
    initCursorPosition = 0

    onLetter :: Letter -> State Double (WithOffset Letter)
    onLetter = advanceBy $ case renderSpacing of
                             Proper _ -> letterOpticalWidth opts
                             Regular  -> const 10

    onSpace :: Letter -> Space -> Letter -> State Double (WithOffset Space)
    onSpace _ s _ = advanceBy (spacing opts) s

    advanceBy :: (a -> Double) -> a -> State Double (WithOffset a)
    advanceBy f x = state $ \cursor -> (x `WithOffset` cursor, cursor + f x)

renderLine :: RenderOptions -> Line -> Diagram B
renderLine opts@RenderOptions{..} line =
      shiftOrigin renderAlignment
    . (if renderRulers then addRulers else id)
    . uncurry atop  -- We are careful to position the letter atop the spaces
    . bimap (foldMap (renderWithOffset $ renderLetter opts))
            (foldMap (renderWithOffset $ renderSpace  opts))
    . Alt.partition
    $ withOffsets
  where
    (withOffsets, totalWidth) = computeOffsets opts line

    shiftOrigin :: Alignment -> Diagram B -> Diagram B
    shiftOrigin AlignLeft   = id
    shiftOrigin AlignCenter = translateX (-0.5 * totalWidth)
    shiftOrigin AlignRight  = translateX (-1.0 * totalWidth)

    addRulers :: Diagram B -> Diagram B
    addRulers l = mconcat [
          l
        , strokePath (fromVertices [p2 (-5, 10), p2 (totalWidth + 5, 10)])
            # lw thin
        , strokePath (fromVertices [p2 (-5,  0), p2 (totalWidth + 5,  0)])
            # lw thin
        ]

-- | Render space
--
-- Origin will be at the bottom left.
renderSpace :: RenderOptions -> Space -> Diagram B
renderSpace opts@RenderOptions{..} s = mconcat [
      -- Rendering grids for spaces gets very confusing, disabled for now
      -- (Primarily because spaces will line up with letter bounds, which
      -- may not be aligned with the grid, and hence the grids of the spaces
      -- will overlap with the grids of the letters)
      -- if renderGrid
      --  then gridOfWidth spaceWidth False
      --  else mempty
      (alignBL $ rect spaceWidth 10)
        # lw none
        # (case renderSpacing of
             Proper True -> fc (spaceColor s)
             _otherwise  -> id)
    ]
  where
    spaceWidth :: Double
    spaceWidth = spacing opts s

    spaceColor :: Space -> Colour Double
    spaceColor InterLetter = pink
    spaceColor InterWord   = lightblue

-- | Render letter
--
-- The origin depends on the render options:
--
-- o If we are doing proper spacing, the origin will be the bottom left bound.
-- o Otherwise, the origin will be the bottom left of the grid.
renderLetter :: RenderOptions -> Letter -> Diagram B
renderLetter opts@RenderOptions{..} l@Letter{..} = mconcat [
      if renderNoLetters
        then mempty
        else strokeLetter opts l
               # lw (intThickness renderThickness)
               # lc renderColour
    , if renderGrid
        then gridOfWidth 10 True
        else mempty
    , if renderBounds
        then bounds
        else mempty
    ] # (case renderSpacing of
           Proper _ -> translateX (-1 * (boundLeft + letterOffset))
           Regular  -> id)
  where
    boundLeft, boundRight :: Double
    (boundLeft, boundRight) = letterBounds (letterCompression opts l)

    bounds :: Diagram B
    bounds = mconcat [
          fromVertices [ p2 (letterOffset + boundLeft, -1)
                       , p2 (letterOffset + boundLeft, 11)
                       ] # lc red
        , fromVertices [ p2 (letterOffset + boundRight, -1)
                       , p2 (letterOffset + boundRight, 11)
                       ] # lc red
        ]

{-------------------------------------------------------------------------------
  Sanitize user input
-------------------------------------------------------------------------------}

newtype Word = Word (NonEmpty Letter)

newtype Error = Error String

parseLine :: String -> Either Error (NonEmpty Word)
parseLine = (>>= toNonEmpty) . mapM parseWord . words
  where
    toNonEmpty :: [Word] -> Either Error (NonEmpty Word)
    toNonEmpty []     = Left $ Error "No words"
    toNonEmpty (w:ws) = Right $ w :| ws

parseWord :: String -> Either Error Word
parseWord []     = Left $ Error "Empty word"
parseWord (x:xs) = Word <$> traverse aux (x :| xs)
  where
    aux :: Char -> Either Error Letter
    aux c'
      | c >= 'A' && c <= 'Z'
      = Right $ trajan c

      | otherwise
      = Left $ Error ("Unexpected character '" ++ [c'] ++ "'")
      where
        c = toUpper c'

