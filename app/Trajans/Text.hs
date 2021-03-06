module Trajans.Text (
    Line(..)
  , Space(..)
  , Alignment(..)
  , RenderOptions(..)
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
import Trajans.Grid

{-------------------------------------------------------------------------------
  Line of text
-------------------------------------------------------------------------------}

newtype Line = Line (Alternate Letter Space)
  deriving (Show)

data Space =
    InterLetter
  | InterWord
  deriving (Show)

spacing :: Space -> Int
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

-- | Compute offsets of all letters and spaces
--
-- Additionally returns the total width of the line (within the bounds)
computeOffsets ::
     Line
  -> (Alternate (WithOffset Letter) (WithOffset Space), Double)
computeOffsets (Line l) =
    flip runState initCursorPosition $
      Alt.traverse onLetter onSpace l
  where
    initCursorPosition :: Double
    initCursorPosition = 0

    onLetter :: Letter -> State Double (WithOffset Letter)
    onLetter = advanceBy letterWidth

    onSpace :: Letter -> Space -> Letter -> State Double (WithOffset Space)
    onSpace _ s _ = advanceBy (fromIntegral . spacing) s

    advanceBy :: (a -> Double) -> a -> State Double (WithOffset a)
    advanceBy f x = state $ \cursor -> (x `WithOffset` cursor, cursor + f x)

data Alignment =
    AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Show)

data RenderOptions = RenderOptions {
      renderAlignment :: Alignment
    , renderGrid      :: Bool
    , renderRulers    :: Bool
    , renderSpaces    :: Bool -- ^ Should spaces be highlighted?
    }
  deriving (Show)

renderLine :: RenderOptions -> Line -> Diagram B
renderLine RenderOptions{..} line =
      shiftOrigin renderAlignment
    . (if renderRulers then addRulers else id)
    . uncurry atop  -- We are careful to position the letter atop the spaces
    . bimap (foldMap (renderWithOffset renderLetter))
            (foldMap (renderWithOffset renderSpace))
    . Alt.partition
    $ withOffsets
  where
    (withOffsets, totalWidth) = computeOffsets line

    shiftOrigin :: Alignment -> Diagram B -> Diagram B
    shiftOrigin AlignLeft   = id
    shiftOrigin AlignCenter = translateX (-0.5 * totalWidth)
    shiftOrigin AlignRight  = translateX (-1.0 * totalWidth)

    renderLetter :: Letter -> Diagram B
    renderLetter l = mconcat [
          strokeLetter l
        , if renderGrid
            then gridOfWidth 10 True
            else mempty
        ] # translateX (-1 * (fst (letterBounds l) + letterOffset l))

    renderSpace :: Space -> Diagram B
    renderSpace s = mconcat [
          if renderGrid
            then gridOfWidth (spacing s) False
            else mempty
        , (alignBL $ rect (fromIntegral (spacing s)) 10)
            # lw none
            # (if renderSpaces then fc (spaceColor s) else id)
        ]

    spaceColor :: Space -> Colour Double
    spaceColor InterLetter = pink
    spaceColor InterWord   = lightblue

    addRulers :: Diagram B -> Diagram B
    addRulers l = mconcat [
          strokePath (fromVertices [p2 (-5, 10), p2 (totalWidth + 5, 10)]) # lw thin
        , l
        , strokePath (fromVertices [p2 (-5,  0), p2 (totalWidth + 5,  0)]) # lw thin
        ]

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

