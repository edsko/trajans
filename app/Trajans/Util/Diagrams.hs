module Trajans.Util.Diagrams (
    -- * Construction
    mkLocTrail
  , uniqueIntersection
  , growBoundingBox
    -- * From the manual
  , illustrateBezier
  ) where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Combinators

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Make located trail from list of vertices, locating it at the first vertex
mkLocTrail :: [Point V2 Double] -> Located (Trail V2 Double)
mkLocTrail vs =
      (`at` head vs)
    . wrapLine
    . fromVertices
    $ vs

-- | Unique intersection between two trails if it exists, undefined otherwise
uniqueIntersection ::
     Located (Trail V2 Double)
  -> Located (Trail V2 Double)
  -> P2 Double
uniqueIntersection t t' =
    case intersectPointsT t t' of
      [i] -> i
      is  -> error $ "uniqueIntersection: not unique: " ++ show is

-- | Grow the bounding box of the diagram by the specified amount
--
-- NOTE: Changes the origin of the diagram to be the center. Intened for
-- usage at the top-level only.
growBoundingBox :: V2 Double -> Diagram B -> Diagram B
growBoundingBox v d = (centerXY d) <> (centerXY (strutR2 bb))
  where
    bb :: V2 Double
    bb = v + boxExtents (boundingBox d)

{-------------------------------------------------------------------------------
  From the manual
-------------------------------------------------------------------------------}

-- | Draw bezier curve with its control points
--
-- From <https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html#segments>
illustrateBezier ::
     V2 Double -- ^ First control point
  -> V2 Double -- ^ Second control point
  -> V2 Double -- ^ End point
  -> Diagram B
illustrateBezier c1 c2 x2 =
       endpt
    <> endpt  # translate x2
    <> ctrlpt # translate c1
    <> ctrlpt # translate c2
    <> l1
    <> l2
    <> fromSegments [bézier3 c1 c2 x2]
  where
    dashed  = dashingN [0.03,0.03] 0
    endpt   = circle 0.05 # fc red  # lw none
    ctrlpt  = circle 0.05 # fc blue # lw none
    l1      = fromOffsets [c1] # dashed
    l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed

