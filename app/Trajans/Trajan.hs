-- | Data defining the individual Trajan capitals
module Trajans.Trajan (trajan) where

import Trajans.Alphabet
import Trajans.Letter

trajan :: Alphabet
trajan = mkAlphabet [
      let width = 8.5
      in Letter {
          letterName    = 'A'
        , letterOffset  = 5
        , letterBounds  = (-2.5, 2)
        , letterStrokes =
            Let (Diagonal (0, 10) (0 - width / 2, 0)) $ \leftLeg ->
            Let (Diagonal (0, 10) (0 + width / 2, 0)) $ \rightLeg ->
            Let (Horizontal (-5, 4) 10) $ \fullCrossBar ->
            mconcat [
                Var leftLeg
              , Var rightLeg
              , Between (Intersection leftLeg fullCrossBar) (Intersection rightLeg fullCrossBar)
              ]
        }
    , let topWidth  = 4.5
          topHeight = 4.75
          topRadius = topHeight / 2
          botWidth  = 5.5
          botHeight = 10 - topHeight
          botRadius = botHeight / 2
          flatten   = 0.95
      in Letter {
          letterName    = 'B'
        , letterOffset  = 1
        , letterBounds  = (0, 4.5)
        , letterStrokes =
            Let (rightHalfCircle (topWidth - topRadius * flatten, 10 - topRadius) topRadius flatten) $ \topCircle ->
            Let (rightHalfCircle (botWidth - botRadius * flatten,  0 + botRadius) botRadius flatten) $ \botCircle ->
            mconcat [
                basic $ Vertical (0, 10) 10
              , Var topCircle
              , Var botCircle
              , 0 `StraightRightTo` Start topCircle
              , 0 `StraightRightTo` Start botCircle
              , 0 `StraightRightTo` End   botCircle
              ]
        }
    , let flatten = 0.95
      in Letter {
          letterName    = 'C'
        , letterOffset  = 5
        , letterBounds  = (-3.5, 2)
        , letterStrokes =
            mconcat [
                basic $ leftHalfCircle (0, 5) 5 flatten
              , basic $ Arc $ MkArc (0, 9.5) 5 (0, 1) (-0.25) (0.8, 0.1)
              , basic $ Arc $ MkArc (0, 0.5) 5 (0, -1) 0.25 (0.8, 0.1)
              ]
        }
    , let flatten = 0.95
      in Letter {
          letterName    = 'D'
        , letterOffset  = 1
        , letterBounds  = (0, 8.5)
        , letterStrokes =
            mconcat [
                basic $ Vertical        (0, 10) 10
              , basic $ Horizontal      (0, 10)  4
              , basic $ Horizontal      (0,  0)  4
              , basic $ rightHalfCircle (4,  5)  5 flatten
              ]
        }
    , Letter {
          letterName    = 'E'
        , letterOffset  = 2
        , letterBounds  = (0, 3.8)
        , letterStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0, 10)  4.0
              , basic $ Horizontal (0,  5)  3.8
              , basic $ Horizontal (0,  0)  4.2
              ]
        }
    , Letter {
          letterName    = 'F'
        , letterOffset  = 2
        , letterBounds  = (0, 3)
        , letterStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0, 10)  4.0
              , basic $ Horizontal (0,  5)  3.8
              ]
        }
    , let flatten = 0.95
      in Letter {
          letterName    = 'G'
        , letterOffset  = 5
        , letterBounds  = (-3.5, 3.5)
        , letterStrokes =
            Let (Arc $ MkArc (0, 0.5) 5 (0, -1) 0.25 (0.8, 0.1)) $ \bottomArc ->
            mconcat [
                basic $ leftHalfCircle (0, 5) 5 flatten
              , basic $ Arc $ MkArc (0, 9.5) 5 (0, 1) (-0.25) (0.8, 0.1)
              , Var bottomArc
              , 5 `StraightDownTo` End bottomArc
              ]
        }
    , Letter {
          letterName    = 'H'
        , letterOffset  = 1
        , letterBounds  = (0, 8)
        , letterStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10
              , basic $ Vertical   (8, 10) 10
              , basic $ Horizontal (0,  5)  8
              ]
        }
    , Letter {
          letterName    = 'I'
        , letterOffset  = 4
        , letterBounds  = (0, 1)
        , letterStrokes =
            mconcat [
                basic $ Vertical (0, 10) 10
              ]
        }
    , let botRadius = 2.5
      in Letter {
          letterName    = 'J'
        , letterOffset  = 9
        , letterBounds  = (-2, 0)
        , letterStrokes =
            Let (Arc $ MkArc (-1 * botRadius, botRadius) botRadius (1, 0) (-0.25) (1, 1)) $ \botArc ->
            mconcat [
                10 `StraightDownTo` Start botArc
              , Var botArc
              , basic $ Arc $ MkArc (-2.5, 0.5) 5 (0, -1) (-0.25) (2 / 5, 0.1)
              ]
        }
    , Letter {
          letterName    = 'K'
        , letterOffset  = 2
        , letterBounds  = (-1, 4.5)
        , letterStrokes = mconcat [
              basic $ Vertical (0, 10) 10
            , basic $ Diagonal (0.25, 5.25) (4.5, 10)
            , basic $ Diagonal (0.25, 5.25) (6.5,  0)
            ]
        }
    , Letter {
          letterName    = 'L'
        , letterOffset  = 1
        , letterBounds  = (0, 2.5)
        , letterStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0,  0)  4.5
              ]
        }
    , Letter {
          letterName    = 'M'
        , letterOffset  = 0
        , letterBounds  = (0, 10)
        , letterStrokes =
            mconcat [
                basic $ Diagonal (-0.5,  0   ) ( 0.5, 10.25)
              , basic $ Diagonal ( 0.5, 10   ) ( 5.0, -0.25)
              , basic $ Diagonal ( 5.0, -0.25) ( 9.5, 10.25)
              , basic $ Diagonal ( 9.5, 10.25) (10.5,  0   )
              ]
        }
    , Letter {
          letterName    = 'N'
        , letterOffset  = 1
        , letterBounds  = (0, 9)
        , letterStrokes =
            mconcat [
                basic $ Vertical (0, 10.25) 10.25
              , basic $ Vertical (9, 10.25) 10.25
              , basic $ Diagonal (0, 10.25) (9, -0.25)
              ]
        }
    , let flatten = 0.95
      in Letter {
          letterName    = 'O'
        , letterOffset  = 5
        , letterBounds  = (-3.7, 3.7)
        , letterStrokes =
            mconcat [
                basic $ Arc $ MkArc (0, 5) 5 (0, 1) 1 (flatten, 1)
              ]
        }
    , let topWidth  = 5.0
          topHeight = 5.5
          topRadius = topHeight / 2
          flatten   = 0.95
      in Letter {
          letterName    = 'P'
        , letterOffset  = 1
        , letterBounds  = (0, 3.5)
        , letterStrokes =
            Let (rightHalfCircle (topWidth - topRadius * flatten, 10 - topRadius) topRadius flatten) $ \topCircle ->
            mconcat [
                basic $ Vertical (0, 10) 10
              , Var topCircle
              , 0 `StraightRightTo` Start topCircle
              , 0 `StraightRightTo` End   topCircle
              ]
        }
    , let flatten = 0.95
      in Letter {
          letterName    = 'Q'
        , letterOffset  = 5
        , letterBounds  = (-3.7, 3.7)
        , letterStrokes =
            Let (Arc $ MkArc (0, 5) 5 (0, 1) 1 (flatten, 1)) $ \oh ->
            Let (Diagonal (-2, 2) (6, -2)) $ \longTail ->
            mconcat [
                Var oh
              , Between (Intersection oh longTail) (End longTail)
              ]
        }
    , let topWidth  = 5.0
          topHeight = 5.5
          topRadius = topHeight / 2
          flatten   = 0.95
      in Letter {
          letterName    = 'R'
        , letterOffset  = 2
        , letterBounds  = (0, 4.5)
        , letterStrokes =
            Let (rightHalfCircle (topWidth - topRadius * flatten, 10 - topRadius) topRadius flatten) $ \topCircle ->
            Let (Diagonal (0, 10) (7, 0)) $ \longLeg ->
            mconcat [
                basic $ Vertical (0, 10) 10
              , Var topCircle
              , 0 `StraightRightTo` Start topCircle
              , 0 `StraightRightTo` End   topCircle
              , Between (Intersection topCircle longLeg) (End longLeg)
              ]
        }
    , let botOffset = 0.35
          midOffset = 0.21 -- how far is the inflection point above the center?
      in Letter {
          letterName    = 'S'
        , letterOffset  = 1
        , letterBounds  = (1, 4)
        , letterStrokes =
            mconcat [
                basic $ DoubleCurve $ MkDoubleCurve {
                    doubleCurveStart       = (0   ,  0 + botOffset)
                  , doubleCurveMid         = (2.5 ,  5 + midOffset)
                  , doubleCurveFinish      = (5   , 10 - botOffset)
                  , doubleCurveControlEnds = (5.35, -1.7)
                  , doubleCurveControlMid  = (4.1, -1.4)
                  , doubleCurveScale1      = (1, 1)
                  , doubleCurveScale2      = (0.95, 1)
                  }
              ]
        }
    , Letter {
          letterName    = 'T'
        , letterOffset  = 1
        , letterBounds  = (2, 6)
        , letterStrokes =
            mconcat [
                basic $ Vertical   (4, 10) 10
              , basic $ Horizontal (0, 10)  8
              ]
        }
    , Letter {
          letterName    = 'U'
        , letterOffset  = 5
        , letterBounds  = (-3.5, 3.5)
        , letterStrokes =
            Let (Arc $ MkArc (0, 3) 4 (-1, 0) 0.5 (1, 3 / 4)) $ \bottomArc ->
            mconcat [
                Var bottomArc
              , 10 `StraightDownTo` Start bottomArc
              , 10 `StraightDownTo` End   bottomArc
              ]
        }
    , let width = 8.4
      in Letter {
          letterName    = 'V'
        , letterOffset  = 5
        , letterBounds  = (-2.3, 2)
        , letterStrokes =
            mconcat [
                basic $ Diagonal (0, -0.25) (-1 * width / 2, 10)
              , basic $ Diagonal (0, -0.25) ( 1 * width / 2, 10)
              ]
        }
    , let width = 7
      in Letter {
          letterName    = 'W'
        , letterOffset  = -2
        , letterBounds  = (2, 12.5)
        , letterStrokes =
            mconcat [
                basic $ Diagonal (0.0 * width, 10.00) (0.5 * width, -0.25)
              , basic $ Diagonal (0.5 * width, -0.25) (1.0 * width, 10.25)
              , basic $ Diagonal (1.0 * width, 10.25) (1.5 * width, -0.25)
              , basic $ Diagonal (1.5 * width, -0.25) (2.0 * width, 10.00)
              ]
        }
    , let topWidth = 5.5
          botWidth = 6.5
      in Letter {
          letterName    = 'X'
        , letterOffset  = 5
        , letterBounds  = (-2, 2)
        , letterStrokes =
            mconcat [
                basic $ Diagonal (-0.5 * topWidth, 10) ( 0.5 * botWidth, 0)
              , basic $ Diagonal ( 0.5 * topWidth, 10) (-0.5 * botWidth, 0)
              ]
        }
    , let width = 7
      in Letter {
          letterName    = 'Y'
        , letterOffset  = 5
        , letterBounds  = (-2.4, 2.2)
        , letterStrokes =
            mconcat [
                basic $ Diagonal (-0.5 * width, 10) (0, 5)
              , basic $ Diagonal ( 0.5 * width, 10) (0, 5)
              , basic $ Vertical (0, 5) 5
              ]
        }
    , let topWidth = 7.7
          botWidth = 8
      in Letter {
          letterName    = 'Z'
        , letterOffset  = 9
        , letterBounds  = (-6.5, -1.5)
        , letterStrokes =
            Let (Horizontal (-1 * topWidth, 10) topWidth) $ \top ->
            Let (Horizontal (-1 * botWidth,  0) botWidth) $ \bot ->
            mconcat [
                Var top
              , Var bot
              , Between (End top) (Start bot)
              ]
        }
    ]

{-------------------------------------------------------------------------------
  Common strokes
-------------------------------------------------------------------------------}

-- | Right half-circle with center, radius, and X-scaling factor
rightHalfCircle :: (Double, Double) -> Double -> Double -> BasicStroke
rightHalfCircle (x, y) r s = Arc MkArc {
      arcCenter = (x, y)
    , arcRadius = r
    , arcStart  = (0, 1)
    , arcLength = -0.5
    , arcScale  = (s, 1)
    }

-- | Right half-circle with center, radius, and X-scaling factor
leftHalfCircle :: (Double, Double) -> Double -> Double -> BasicStroke
leftHalfCircle (x, y) r s = Arc MkArc {
      arcCenter = (x, y)
    , arcRadius = r
    , arcStart  = (0, 1)
    , arcLength = 0.5
    , arcScale  = (s, 1)
    }

