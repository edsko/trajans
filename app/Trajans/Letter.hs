module Trajans.Letter (
    -- * Internal letter representation
    Letter(..)
  , strokeLetter
    -- * Letter specification
  , Spec(..)
  , mkLetter
    -- * Strokes
  , BasicStroke(..)
  , Arc(..)
  , Strokes(..)
  , Vertex(..)
  , basic
  ) where

import Data.Functor.Const
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (Const(..), Empty, Start, End, arcLength)

import Trajans.Util.Diagrams

{-------------------------------------------------------------------------------
  Internal letter representation
-------------------------------------------------------------------------------}

data Letter = Letter {
      letterPath :: Path V2 Double
    }

strokeLetter :: Letter -> Diagram B
strokeLetter = strokePath . letterPath

{-------------------------------------------------------------------------------
  Letter specification
-------------------------------------------------------------------------------}

-- | Letter specification, as we want to write it when defining the alphabet
data Spec = Spec {
      -- | Which letter of the alpabet is this?
      specLetter :: Char

      -- | The offset from the left margin of the letter
    , specOffset :: Double

      -- | The lines of the letter, relative to 'specOffset'
    , specStrokes :: forall f. Strokes f
    }

mkLetter :: Spec -> Letter
mkLetter Spec{..} = Letter {
      letterPath = Path (intStrokes specStrokes) # translateX specOffset
    }

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

data Arc = MkArc {
      arcCenter :: (Double, Double)
    , arcRadius :: Double
    , arcStart  :: (Double, Double) -- ^ Vector pointing to the start
    , arcLength :: Double           -- ^ In number of full turns
    , arcScale  :: (Double, Double) -- ^ Applied /before/ translation
    }

intStroke :: BasicStroke -> Located (Trail V2 Double)
intStroke = \case
    Horizontal (x, y) len ->
      mkLocTrail [p2 (x, y), p2 (x + len, y)]
    Vertical (x, y) len ->
      mkLocTrail [p2 (x, y), p2 (x, y - len)]
    Diagonal (x, y) (x', y') ->
      mkLocTrail [p2 (x, y), p2 (x', y')]
    Arc MkArc{..} ->
      arc' arcRadius (direction (r2 arcStart)) (arcLength @@ turn)
        # scaleX (fst arcScale)
        # scaleY (snd arcScale)
        # translate (r2 arcCenter)

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

data Vertex f =
    Intersection (f BasicStroke) (f BasicStroke)
  | Start (f BasicStroke)
  | End (f BasicStroke)

basic :: BasicStroke -> Strokes f
basic s = Let s Var

instance Monoid    (Strokes f) where mempty = Empty
instance Semigroup (Strokes f) where (<>)   = Append

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

