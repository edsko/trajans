module Trajans.Letter (
    -- * Letter definition
    Letter(..)
  , Compression(..)
  , letterCompression
  , letterOpticalWidth
  , strokeLetter
    -- * Strokes
  , BasicStroke(..)
  , Arc(..)
  , DoubleCurve(..)
  , Strokes(..)
  , Vertex(..)
  , basic
  ) where

import Data.Functor.Const
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (Const(..), Empty, Start, End, arcLength)

import Trajans.RenderOptions
import Trajans.Util.Diagrams

{-------------------------------------------------------------------------------
  Letter definition
-------------------------------------------------------------------------------}

-- | Letter specification, as we want to write it when defining the alphabet
data Letter = Letter {
      -- | Which letter of the alpabet is this?
      letterName :: Char

      -- | The offset from the left margin of the letter
    , letterOffset :: Double

      -- | Regular (uncompressed) width of the letter
    , letterWidth :: Double

      -- | Compressed width of the letter
    , letterXCompr :: Double

      -- | The bounds of the letter for spacing, relative to 'letterOffset'
      --
      -- Gets the compression factor as an argument.
    , letterBounds :: Compression -> (Double, Double)

      -- | The lines of the letter, relative to 'letterOffset'
      --
      -- Gets the compression factor as an argument.
    , letterStrokes :: Compression -> forall f. Strokes f
    }

-- | Compression factor (actual width over width)
newtype Compression = Compression Double

instance Show Letter where
  show Letter{letterName} = show letterName

letterCompression :: RenderOptions -> Letter -> Compression
letterCompression RenderOptions{..} Letter{..}
  | letterWidth == 0 = Compression $ 1
  | renderXCompr     = Compression $ letterXCompr / letterWidth
  | otherwise        = Compression $ 1

-- | The distance between the 'letterBounds'
letterOpticalWidth :: RenderOptions -> Letter -> Double
letterOpticalWidth opts l@Letter{..} =
    x' - x
  where
    x, x' :: Double
    (x, x') = letterBounds (letterCompression opts l)

-- | Shroke a letter
--
-- Shearing is not supported in debug mode.
strokeLetter :: RenderOptions -> Letter -> Diagram B
strokeLetter opts@RenderOptions{..} l@Letter{..} =
    rendered # translateX letterOffset
  where
    rendered :: Diagram B
    rendered
      | renderDebug = debugStrokes strokes
      | otherwise   = strokePath $ Path (intStrokes strokes)
                                     # shearX shearDistance

    shearDistance :: Double
    shearDistance = 1 / tan ((90 - renderSlope) * (pi/180))

    strokes :: Strokes f
    strokes = letterStrokes (letterCompression opts l)

{-------------------------------------------------------------------------------
  Single stroke of a letter
-------------------------------------------------------------------------------}

-- | Stroke of a letter
--
-- " Origin " here is always interpreted relative to 'specOffset'
data BasicStroke =
    -- | Horizontal stroke with left origin and length
    Horizontal (Double, Double) Double

    -- | Vertical stroke with top origin and length
  | Vertical (Double, Double) Double

    -- | Diagonal stroke with begin and end point
  | Diagonal (Double, Double) (Double, Double)

    -- | Arc with start and end position in degrees, radius, and X- and Y- scaling
  | Arc Arc

    -- | Symmetric double curve between three points with two control points
  | DoubleCurve DoubleCurve

intStroke :: BasicStroke -> Located (Trail V2 Double)
intStroke (Horizontal (x, y) len)    = mkLocTrail [p2 (x, y), p2 (x + len, y)]
intStroke (Vertical   (x, y) len)    = mkLocTrail [p2 (x, y), p2 (x, y - len)]
intStroke (Diagonal (x, y) (x', y')) = mkLocTrail [p2 (x, y), p2 (x', y')]
intStroke (Arc a)                    = intArc a
intStroke (DoubleCurve c)            = intDoubleCurve c

-- | Show how the stroke is constructed
--
-- Currently we only do anything special for 'DoubleCurve'.
debugStroke :: BasicStroke -> Diagram B
debugStroke (DoubleCurve c) = debugDoubleCurve c
debugStroke s               = strokePath $ Path [intStroke s]

{-------------------------------------------------------------------------------
  Arc
-------------------------------------------------------------------------------}

data Arc = MkArc {
      arcCenter :: (Double, Double)
    , arcRadius :: Double
    , arcStart  :: (Double, Double) -- ^ Vector pointing to the start
    , arcLength :: Double           -- ^ In number of full turns
    , arcScale  :: (Double, Double) -- ^ Applied /before/ translation
    }

intArc :: Arc -> Located (Trail V2 Double)
intArc MkArc{..} =
    arc' arcRadius (direction (r2 arcStart)) (arcLength @@ turn)
      # scaleX (fst arcScale)
      # scaleY (snd arcScale)
      # translate (r2 arcCenter)

{-------------------------------------------------------------------------------
  Double curve
-------------------------------------------------------------------------------}

-- | Symmetric double curve
data DoubleCurve = MkDoubleCurve {
      doubleCurveStart  :: (Double, Double)
    , doubleCurveMid    :: (Double, Double)
    , doubleCurveFinish :: (Double, Double)

      -- | Control for the start and (rotated 180) for the finish
      --
      -- This is interpreted relative to 'doubleCurveStart'
    , doubleCurveControlEnds :: (Double, Double)

      -- | Control for the mid point (twice, once rotated 180)
      --
      -- This is interpreted relative to 'doubleCurveMid'
    , doubleCurveControlMid :: (Double, Double)

      -- | Scaling for the first curve
    , doubleCurveScale1 :: (Double, Double)

      -- | Scaling for the second curve
    , doubleCurveScale2 :: (Double, Double)
    }

intDoubleCurve :: DoubleCurve -> Located (Trail V2 Double)
intDoubleCurve dc = dcdDoubleCurve
  where
   DoubleCurveData{..} = doubleCurveData dc

-- | Show the endpoints and control points of the double curve
--
-- This is adapted from 'illustrateBezier'.
debugDoubleCurve :: DoubleCurve -> Diagram B
debugDoubleCurve dc@MkDoubleCurve{..} = mconcat [
     foldMap (\p -> endpt # translate p) [
         r2 doubleCurveStart
       , r2 doubleCurveMid
       , r2 doubleCurveFinish
       ]
   , foldMap (\(v, v') -> ctrlpt # translate (v + v')) [
          (r2 doubleCurveStart  , dcdC1)
        , (r2 doubleCurveMid    , dcdC2)
        , (r2 doubleCurveMid    , dcdC3)
        , (r2 doubleCurveFinish , dcdC4)
        ]
   , foldMap (\t -> strokePath t # dashed) [l1, l2, l3, l4]
   , strokePath (Path [dcdDoubleCurve]) # lc purple
   ]
 where
   DoubleCurveData{..} = doubleCurveData dc

   dashed :: Diagram B -> Diagram B
   dashed = dashingN [0.03,0.03] 0

   endpt, ctrlpt :: Diagram B
   endpt  = circle 0.1 # fc red  # lw none
   ctrlpt = circle 0.1 # fc blue # lw none

   l1, l2, l3, l4 :: Path V2 Double
   l1 = fromVertices [p2 doubleCurveStart  , p2 doubleCurveStart  .+^ dcdC1]
   l2 = fromVertices [p2 doubleCurveMid    , p2 doubleCurveMid    .+^ dcdC2]
   l3 = fromVertices [p2 doubleCurveMid    , p2 doubleCurveMid    .+^ dcdC3]
   l4 = fromVertices [p2 doubleCurveFinish , p2 doubleCurveFinish .+^ dcdC4]

-- | Internal: data derived from the double curve
data DoubleCurveData = DoubleCurveData {
      -- Control points
      dcdC1, dcdC2, dcdC3, dcdC4 :: V2 Double

      -- The curve itself
    , dcdDoubleCurve :: Located (Trail V2 Double)
    }

doubleCurveData :: DoubleCurve -> DoubleCurveData
doubleCurveData MkDoubleCurve{..} = DoubleCurveData {..}
  where
    -- Control points
    dcdC1 = r2 doubleCurveControlEnds
    dcdC2 = r2 doubleCurveControlMid
    dcdC3 = rotateBy 0.5 dcdC2
    dcdC4 = rotateBy 0.5 dcdC1

    -- Internal: vectors used to construct the curves
    startToMid :: V2 Double
    startToMid  = r2 doubleCurveMid    - r2 doubleCurveStart
    midToFinish = r2 doubleCurveFinish - r2 doubleCurveMid

    -- Internal: The two halves of the curves
    curveBottom, curveTop :: Segment Closed V2 Double
    curveBottom = scaleX (fst doubleCurveScale1)
                $ scaleY (snd doubleCurveScale2)
                $ bezier3 dcdC1 (dcdC2 + startToMid)  startToMid
    curveTop    = scaleX (fst doubleCurveScale2)
                $ scaleY (snd doubleCurveScale2)
                $ bezier3 dcdC3 (dcdC4 + midToFinish) midToFinish

    -- The double curve itself
    dcdDoubleCurve = fromSegments [curveBottom, curveTop]
                       # translate (r2 doubleCurveStart)

{-------------------------------------------------------------------------------
  Multiple strokes, with sharing
-------------------------------------------------------------------------------}

data Strokes f =
    Empty
  | Append (Strokes f) (Strokes f)
  | Let BasicStroke (f BasicStroke -> Strokes f)
  | Var (f BasicStroke)
  | Between (Vertex f) (Vertex f)
  | StraightRightTo Double (Vertex f)
  | StraightDownTo Double (Vertex f)

instance Monoid    (Strokes f) where mempty = Empty
instance Semigroup (Strokes f) where (<>)   = Append

data Vertex f =
    Intersection (f BasicStroke) (f BasicStroke)
  | Start (f BasicStroke)
  | End (f BasicStroke)

basic :: BasicStroke -> Strokes f
basic s = Let s Var

-- | Interpret 'Strokes'
--
-- We turn every stroke into a one-segment trail.
intStrokes :: Strokes (Const (Located (Trail V2 Double))) -> [Located (Trail V2 Double)]
intStrokes = \case
    Empty ->
      mempty
    Append ss ss' ->
      intStrokes ss <> intStrokes ss'
    Let s f ->
      intStrokes (f (Const (intStroke s)))
    Var (Const x) -> pure $
      x
    Between (intVertex -> v) (intVertex -> v') -> pure $
      fromVertices [v, v']
    StraightRightTo x (intVertex -> v) -> pure $
      fromVertices [v & _x .~ x, v]
    StraightDownTo y (intVertex -> v) -> pure $
      fromVertices [v & _y .~ y, v]

intVertex :: Vertex (Const (Located (Trail V2 Double))) -> P2 Double
intVertex (Intersection (Const t) (Const t')) = uniqueIntersection t t'
intVertex (Start        (Const t)           ) = head (trailVertices t)
intVertex (End          (Const t)           ) = last (trailVertices t)

-- | Debugging currently ignores non-basic strokes
debugStrokes :: Strokes (Const (Diagram B)) -> Diagram B
debugStrokes  Empty                = mempty
debugStrokes (Append ss ss')       = debugStrokes ss <> debugStrokes ss'
debugStrokes (Let s f)             = debugStrokes (f (Const (debugStroke s)))
debugStrokes (Var (Const x))       = x
debugStrokes (Between _ _)         = mempty
debugStrokes (StraightRightTo _ _) = mempty
debugStrokes (StraightDownTo _ _)  = mempty



