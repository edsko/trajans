-- | Data defining the individual Trajan capitals
module Trajans.Trajan (trajan) where

import Trajans.Alphabet
import Trajans.Letter

trajan :: Alphabet
trajan = mkAlphabet [
      let regularWidth = 8.5 in
      Letter {
          letterName    = 'A'
        , letterOffset  = 5
        , letterWidth   = regularWidth
        , letterXCompr  = 6.5
        , letterBounds  = \(Compression c) ->
            (-2.5 * c, 2 * c)
        , letterStrokes = \(Compression c) ->
            let width = c * regularWidth in
            Let (Diagonal (0, 10) (0 - width / 2, 0)) $ \leftLeg ->
            Let (Diagonal (0, 10) (0 + width / 2, 0)) $ \rightLeg ->
            Let (Horizontal (-5, 4) 10) $ \fullCrossBar ->
            mconcat [
                Var leftLeg
              , Var rightLeg
              , Between (Intersection leftLeg fullCrossBar) (Intersection rightLeg fullCrossBar)
              ]
        }
    , Letter {
          letterName    = 'B'
        , letterOffset  = 1
        , letterWidth   = 5.5
        , letterXCompr  = 3.5
        , letterBounds  = \(Compression c) -> (0, c * 4.5)
        , letterStrokes = \(Compression c) ->
            let topWidth  = c * 4.5
                botWidth  = c * 5.5
                flatten   = c * 0.95
                topHeight = 4.75
                topRadius = topHeight / 2
                botHeight = 10 - topHeight
                botRadius = botHeight / 2
            in
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
    , Letter {
          letterName    = 'C'
        , letterOffset  = 5
        , letterWidth   = 8.8
        , letterXCompr  = 5.5
        , letterBounds  = \(Compression c) -> (-3.5 * c, 2 * c)
        , letterStrokes = \(Compression c) ->
            let flattenLeft  = c * 0.95
                flattenRight = c * 0.8
            in
            mconcat [
                basic $ leftHalfCircle (0, 5) 5 flattenLeft
              , basic $ Arc $ MkArc (0, 9.5) 5 (0, 1) (-0.25) (flattenRight, 0.1)
              , basic $ Arc $ MkArc (0, 0.5) 5 (0, -1) 0.25 (flattenRight, 0.1)
              ]
        }
    , Letter {
          letterName    = 'D'
        , letterOffset  = 1
        , letterWidth   = 8.8
        , letterXCompr  = 5.5
        , letterBounds  = \(Compression c) -> (0, c * 7.5)
        , letterStrokes = \(Compression c) ->
            let flattenRight = c * 0.95
                widthLeft    = c * 4
            in mconcat [
                basic $ Vertical   (0, 10) 10
              , basic $ Horizontal (0, 10) widthLeft
              , basic $ Horizontal (0,  0) widthLeft
              , basic $ rightHalfCircle (widthLeft, 5) 5 flattenRight
              ]
        }
    , Letter {
          letterName    = 'E'
        , letterOffset  = 2
        , letterWidth   = 4.2
        , letterXCompr  = 4
        , letterBounds  = \(Compression c) -> (0, c * 3.8)
        , letterStrokes = \(Compression c) ->
            let widthTop = c * 4.0
                widthMid = c * 3.8
                widthBot = c * 4.2
            in mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0, 10) widthTop
              , basic $ Horizontal (0,  5) widthMid
              , basic $ Horizontal (0,  0) widthBot
              ]
        }
    , Letter {
          letterName    = 'F'
        , letterOffset  = 2
        , letterWidth   = 4
        , letterXCompr  = 3.5
        , letterBounds  = \(Compression c) -> (0, c * 3)
        , letterStrokes = \(Compression c) ->
            let widthTop = c * 4
                widthMid = c * 3.8
            in mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0, 10) widthTop
              , basic $ Horizontal (0,  5) widthMid
              ]
        }
    , Letter {
          letterName    = 'G'
        , letterOffset  = 5
        , letterWidth   = 8.8
        , letterXCompr  = 5.5
        , letterBounds  = \(Compression c) -> (c * (-3.5), c * 3.5)
        , letterStrokes = \(Compression c) ->
            let flattenLeft  = c * 0.95
                flattenRight = c * 0.8
            in
            Let (Arc $ MkArc (0, 0.5) 5 (0, -1) 0.25 (flattenRight, 0.1)) $ \bottomArc ->
            mconcat [
                basic $ leftHalfCircle (0, 5) 5 flattenLeft
              , basic $ Arc $ MkArc (0, 9.5) 5 (0, 1) (-0.25) (flattenRight, 0.1)
              , Var bottomArc
              , 5 `StraightDownTo` End bottomArc
              ]
        }
    , Letter {
          letterName    = 'H'
        , letterOffset  = 1
        , letterWidth   = 8
        , letterXCompr  = 5
        , letterBounds  = \(Compression c) -> (0, c * 8)
        , letterStrokes = \(Compression c) ->
            let width = c * 8 in
            mconcat [
                basic $ Vertical   (0,     10) 10
              , basic $ Vertical   (width, 10) 10
              , basic $ Horizontal (0,      5) width
              ]
        }
    , Letter {
          letterName    = 'I'
        , letterOffset  = 4
        , letterWidth   = 0
        , letterXCompr  = 0
        , letterBounds  = \(Compression _) -> (0, 1)
        , letterStrokes = \(Compression _) ->
            mconcat [
                basic $ Vertical (0, 10) 10
              ]
        }
    , Letter {
          letterName    = 'J'
        , letterOffset  = 9
        , letterWidth   = 4.5
        , letterXCompr  = 3.5 -- Yves does not provide this value (different J)
        , letterBounds  = \(Compression c) -> (c * (-2), 0)
        , letterStrokes = \(Compression c) ->
            let botRadius = 2.5
                flattenRight = c
                flattenLeft  = c * (2 / 5)
            in
            Let (Arc $ MkArc (-1 * botRadius, botRadius) botRadius (1, 0) (-0.25) (flattenRight, 1)) $ \botArc ->
            mconcat [
                10 `StraightDownTo` Start botArc
              , Var botArc
              , basic $ Arc $ MkArc (-2.5, 0.5) 5 (0, -1) (-0.25) (flattenLeft, 0.1)
              ]
        }
    , Letter {
          letterName    = 'K'
        , letterOffset  = 2
        , letterWidth   = 6.5
        , letterXCompr  = 5.5
        , letterBounds  = \(Compression c) -> (c * (-1), c * 4.5)
        , letterStrokes = \(Compression c) -> mconcat [
              basic $ Vertical (0, 10) 10
            , basic $ Diagonal (c * 0.25, 5.25) (c * 4.5, 10)
            , basic $ Diagonal (c * 0.25, 5.25) (c * 6.5,  0)
            ]
        }
    , Letter {
          letterName    = 'L'
        , letterOffset  = 1
        , letterWidth   = 4.5
        , letterXCompr  = 4
        , letterBounds  = \(Compression c) -> (0, c * 2.5)
        , letterStrokes = \(Compression c) ->
            mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0,  0) (c * 4.5)
              ]
        }
    , Letter {
          letterName    = 'M'
        , letterOffset  = 0
        , letterWidth   = 11
        , letterXCompr  = 9
        , letterBounds  = \(Compression c) -> (0, c * 10)
        , letterStrokes = \(Compression c) ->
            mconcat [
                basic $ Diagonal (c * (-0.5),  0   ) (c *  0.5, 10.25)
              , basic $ Diagonal (c *   0.5 , 10   ) (c *  5.0, -0.25)
              , basic $ Diagonal (c *   5.0 , -0.25) (c *  9.5, 10.25)
              , basic $ Diagonal (c *   9.5 , 10.25) (c * 10.5,  0   )
              ]
        }
    , Letter {
          letterName    = 'N'
        , letterOffset  = 1
        , letterWidth   = 9
        , letterXCompr  = 5.5
        , letterBounds  = \(Compression c) -> (0, c * 9)
        , letterStrokes = \(Compression c) ->
            let width = c * 9 in
            mconcat [
                basic $ Vertical (    0, 10.25) 10.25
              , basic $ Vertical (width, 10.25) 10.25
              , basic $ Diagonal (    0, 10.25) (width, -0.25)
              ]
        }
    , Letter {
          letterName    = 'O'
        , letterOffset  = 5
        , letterWidth   = 9.7
        , letterXCompr  = 6.5
        , letterBounds  = \(Compression c) -> (c * (-3.7), c * 3.7)
        , letterStrokes = \(Compression c) ->
            let flatten = c * 0.95 in
            mconcat [
                basic $ Arc $ MkArc (0, 5) 5 (0, 1) 1 (flatten, 1)
              ]
        }
    , Letter {
          letterName    = 'P'
        , letterOffset  = 2
        , letterWidth   = 5
        , letterXCompr  = 3.5
        , letterBounds  = \(Compression c) -> (0, c * 3.5)
        , letterStrokes = \(Compression c) ->
            let topWidth  = c * 5.0
                flatten   = c * 0.95
                topHeight = 5.5
                topRadius = topHeight / 2
            in
            Let (rightHalfCircle (topWidth - topRadius * flatten, 10 - topRadius) topRadius flatten) $ \topCircle ->
            mconcat [
                basic $ Vertical (0, 10) 10
              , Var topCircle
              , 0 `StraightRightTo` Start topCircle
              , 0 `StraightRightTo` End   topCircle
              ]
        }
    , Letter {
          letterName    = 'Q'
        , letterOffset  = 5
        , letterWidth   = 9.7
        , letterXCompr  = 6.5
        , letterBounds  = \(Compression c) -> (c * (-3.7), c * 3.7)
        , letterStrokes = \(Compression c) ->
            let flatten = c * 0.95 in
            Let (Arc $ MkArc (0, 5) 5 (0, 1) 1 (flatten, 1)) $ \oh ->
            Let (Diagonal (c * (-2), 2) (c * 6, -2)) $ \longTail ->
            mconcat [
                Var oh
              , Between (Intersection oh longTail) (End longTail)
              ]
        }
    , Letter {
          letterName    = 'R'
        , letterOffset  = 2
        , letterWidth   = 7
        , letterXCompr  = 5
        , letterBounds  = \(Compression c) -> (0, c * 4.5)
        , letterStrokes = \(Compression c) ->
            let topWidth  = c * 5.0
                flatten   = c * 0.95
                topHeight = 5.5
                topRadius = topHeight / 2
            in
            Let (rightHalfCircle (topWidth - topRadius * flatten, 10 - topRadius) topRadius flatten) $ \topCircle ->
            Let (Diagonal (0, 10) (c * 7, 0)) $ \longLeg ->
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
        , letterOffset  = 4.5
        , letterWidth   = 5
        , letterXCompr  = 3.5
        , letterBounds  = \(Compression c) -> (c * (-1.5), c * 1.5)
        , letterStrokes = \(Compression c) ->
            mconcat [
                basic $ DoubleCurve $ MkDoubleCurve {
                    doubleCurveStart       = (c * (-2.5),  0 + botOffset)
                  , doubleCurveMid         = (c *    0  ,  5 + midOffset)
                  , doubleCurveFinish      = (c *  2.5  , 10 - botOffset)
                  , doubleCurveControlEnds = (c *  5.35 , -1.7) -- relative
                  , doubleCurveControlMid  = (c *  4.1  , -1.4)
                  , doubleCurveScale1      = (1    , 1)
                  , doubleCurveScale2      = (0.95 , 1)
                  }
              ]
        }
    , Letter {
          letterName    = 'T'
        , letterOffset  = 1
        , letterWidth   = 8
        , letterXCompr  = 6
        , letterBounds  = \(Compression c) -> (c * 2, c * 6)
        , letterStrokes = \(Compression c) ->
            let width = c * 8 in
            mconcat [
                basic $ Vertical (width / 2, 10) 10
              , basic $ Horizontal (0, 10) width
              ]
        }
    , Letter {
          letterName    = 'U'
        , letterOffset  = 5
        , letterWidth   = 8
        , letterXCompr  = 5.5
        , letterBounds  = \(Compression c) -> (c * (-3.5), c * 3.5)
        , letterStrokes = \(Compression c) ->
            Let (Arc $ MkArc (0, 3) 4 (-1, 0) 0.5 (c, 3 / 4)) $ \bottomArc ->
            mconcat [
                Var bottomArc
              , 10 `StraightDownTo` Start bottomArc
              , 10 `StraightDownTo` End   bottomArc
              ]
        }
    , Letter {
          letterName    = 'V'
        , letterOffset  = 5
        , letterWidth   = 8.4
        , letterXCompr  = 6.5
        , letterBounds  = \(Compression c) -> (c * (-2.3), c * 2)
        , letterStrokes = \(Compression c) ->
            let width = c * 8.4 in
            mconcat [
                basic $ Diagonal (0, -0.25) (-1 * width / 2, 10)
              , basic $ Diagonal (0, -0.25) ( 1 * width / 2, 10)
              ]
        }
    , Letter {
          letterName    = 'W'
        , letterOffset  = 5
        , letterWidth   = 14
        , letterXCompr  = 12.5
        , letterBounds  = \(Compression c) -> (c * (-5), c * 5.5)
        , letterStrokes = \(Compression c) ->
            let width = c * 7 in
            mconcat [
                basic $ Diagonal (-1   * width, 10.00) (-0.5 * width, -0.25)
              , basic $ Diagonal (-0.5 * width, -0.25) ( 0   * width, 10.25)
              , basic $ Diagonal ( 0   * width, 10.25) ( 0.5 * width, -0.25)
              , basic $ Diagonal ( 0.5 * width, -0.25) ( 1.0 * width, 10.00)
              ]
        }
    , Letter {
          letterName    = 'X'
        , letterOffset  = 5
        , letterWidth   = 6.5
        , letterXCompr  = 6
        , letterBounds  = \(Compression c) -> (c * (-2), c * 2)
        , letterStrokes = \(Compression c) ->
            let topWidth = c * 5.5
                botWidth = c * 6.5
            in
            mconcat [
                basic $ Diagonal (-0.5 * topWidth, 10) ( 0.5 * botWidth, 0)
              , basic $ Diagonal ( 0.5 * topWidth, 10) (-0.5 * botWidth, 0)
              ]
        }
    , Letter {
          letterName    = 'Y'
        , letterOffset  = 5
        , letterWidth   = 7
        , letterXCompr  = 6
        , letterBounds  = \(Compression c) -> (c * (-2.4), c * 2.2)
        , letterStrokes = \(Compression c) ->
            let width = c * 7 in
            mconcat [
                basic $ Diagonal (-0.5 * width, 10) (0, 5)
              , basic $ Diagonal ( 0.5 * width, 10) (0, 5)
              , basic $ Vertical (0, 5) 5
              ]
        }
    , Letter {
          letterName    = 'Z'
        , letterOffset  = 9
        , letterWidth   = 8
        , letterXCompr  = 6
        , letterBounds  = \(Compression c) -> (c * (-6.5), c * (-1.5))
        , letterStrokes = \(Compression c) ->
            let topWidth = c * 7.7
                botWidth = c * 8
            in
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

