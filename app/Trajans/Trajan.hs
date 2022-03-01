-- | Data defining the individual Trajan capitals
module Trajans.Trajan (trajan) where

import Trajans.Alphabet
import Trajans.Letter

trajan :: Alphabet
trajan = mkAlphabet [
      let width = 8.5
      in Spec {
          specLetter  = 'A'
        , specOffset  = 5
        , specStrokes =
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
      in Spec {
          specLetter  = 'B'
        , specOffset  = 1
        , specStrokes =
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
      in Spec {
          specLetter  = 'C'
        , specOffset  = 5
        , specStrokes =
            mconcat [
                basic $ leftHalfCircle (0, 5) 5 flatten
              , basic $ Arc $ MkArc (0, 9.5) 5 (0, 1) (-0.25) (0.8, 0.1)
              , basic $ Arc $ MkArc (0, 0.5) 5 (0, -1) 0.25 (0.8, 0.1)
              ]
        }
    , let flatten = 0.95
      in Spec {
          specLetter  = 'D'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical        (0, 10) 10
              , basic $ Horizontal      (0, 10)  4
              , basic $ Horizontal      (0,  0)  4
              , basic $ rightHalfCircle (4,  5)  5 flatten
              ]
        }
    , Spec {
          specLetter  = 'E'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0, 10)  4.0
              , basic $ Horizontal (0,  5)  3.8
              , basic $ Horizontal (0,  0)  4.2
              ]
        }
    , Spec {
          specLetter  = 'F'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0, 10)  4.0
              , basic $ Horizontal (0,  5)  3.8
              ]
        }
    , let flatten = 0.95
      in Spec {
          specLetter  = 'G'
        , specOffset  = 5
        , specStrokes =
            Let (Arc $ MkArc (0, 0.5) 5 (0, -1) 0.25 (0.8, 0.1)) $ \bottomArc ->
            mconcat [
                basic $ leftHalfCircle (0, 5) 5 flatten
              , basic $ Arc $ MkArc (0, 9.5) 5 (0, 1) (-0.25) (0.8, 0.1)
              , Var bottomArc
              , 5 `StraightDownTo` End bottomArc
              ]
        }
    , Spec {
          specLetter  = 'H'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10
              , basic $ Vertical   (8, 10) 10
              , basic $ Horizontal (0,  5)  8
              ]
        }
    , Spec {
          specLetter  = 'I'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical (0, 10) 10
              ]
        }
    , let botRadius = 2.5
      in Spec {
          specLetter  = 'J'
        , specOffset  = 9
        , specStrokes =
            Let (Arc $ MkArc (-1 * botRadius, botRadius) botRadius (1, 0) (-0.25) (1, 1)) $ \botArc ->
            mconcat [
                10 `StraightDownTo` Start botArc
              , Var botArc
              , basic $ Arc $ MkArc (-2.5, 0.5) 5 (0, -1) (-0.25) (2 / 5, 0.1)
              ]
        }
    , Spec {
          specLetter  = 'K'
        , specOffset  = 1
        , specStrokes = mconcat [
              basic $ Vertical (0, 10) 10
            , basic $ Diagonal (0.25, 5.25) (4.5, 10)
            , basic $ Diagonal (0.25, 5.25) (6.5,  0)
            ]
        }
    , Spec {
          specLetter  = 'L'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical   (0, 10) 10.0
              , basic $ Horizontal (0,  0)  4.5
              ]
        }
    , Spec {
          specLetter  = 'M'
        , specOffset  = 0
        , specStrokes =
            mconcat [
                basic $ Diagonal (-0.5,  0   ) ( 0.5, 10.25)
              , basic $ Diagonal ( 0.5, 10   ) ( 5.0, -0.25)
              , basic $ Diagonal ( 5.0, -0.25) ( 9.5, 10.25)
              , basic $ Diagonal ( 9.5, 10.25) (10.5,  0   )
              ]
        }
    , Spec {
          specLetter  = 'N'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical (0, 10.25) 10.25
              , basic $ Vertical (9, 10.25) 10.25
              , basic $ Diagonal (0, 10.25) (9, -0.25)
              ]
        }
    , let flatten = 0.95
      in Spec {
          specLetter  = 'O'
        , specOffset  = 5
        , specStrokes =
            mconcat [
                basic $ Arc $ MkArc (0, 5) 5 (0, 1) 1 (flatten, 1)
              ]
        }
    , let topWidth  = 5.0
          topHeight = 5.5
          topRadius = topHeight / 2
          flatten   = 0.95
      in Spec {
          specLetter  = 'P'
        , specOffset  = 1
        , specStrokes =
            Let (rightHalfCircle (topWidth - topRadius * flatten, 10 - topRadius) topRadius flatten) $ \topCircle ->
            mconcat [
                basic $ Vertical (0, 10) 10
              , Var topCircle
              , 0 `StraightRightTo` Start topCircle
              , 0 `StraightRightTo` End   topCircle
              ]
        }
    , Spec {
          specLetter  = 'Q'
        , specOffset  = 0
        , specStrokes = mconcat [] -- TODO
        }
    , Spec {
          specLetter  = 'R'
        , specOffset  = 0
        , specStrokes = mconcat [] -- TODO
        }
    , Spec {
          specLetter  = 'S'
        , specOffset  = 0
        , specStrokes = mconcat [] -- TODO
        }
    , Spec {
          specLetter  = 'T'
        , specOffset  = 1
        , specStrokes =
            mconcat [
                basic $ Vertical   (4, 10) 10
              , basic $ Horizontal (0, 10)  8
              ]
        }
    , Spec {
          specLetter  = 'U'
        , specOffset  = 5
        , specStrokes =
            Let (Arc $ MkArc (0, 3) 4 (-1, 0) 0.5 (1, 3 / 4)) $ \bottomArc ->
            mconcat [
                Var bottomArc
              , 10 `StraightDownTo` Start bottomArc
              , 10 `StraightDownTo` End   bottomArc
              ]
        }
    , let width = 8.4
      in Spec {
          specLetter  = 'V'
        , specOffset  = 5
        , specStrokes =
            mconcat [
                basic $ Diagonal (0, -0.25) (-1 * width / 2, 10)
              , basic $ Diagonal (0, -0.25) ( 1 * width / 2, 10)
              ]
        }
    , let width = 7
      in Spec {
          specLetter  = 'W'
        , specOffset  = -2
        , specStrokes =
            mconcat [
                basic $ Diagonal (0.0 * width, 10.00) (0.5 * width, -0.25)
              , basic $ Diagonal (0.5 * width, -0.25) (1.0 * width, 10.25)
              , basic $ Diagonal (1.0 * width, 10.25) (1.5 * width, -0.25)
              , basic $ Diagonal (1.5 * width, -0.25) (2.0 * width, 10.00)
              ]
        }
    , let topWidth = 5.5
          botWidth = 6.5
      in Spec {
          specLetter  = 'X'
        , specOffset  = 5
        , specStrokes =
            mconcat [
                basic $ Diagonal (-0.5 * topWidth, 10) ( 0.5 * botWidth, 0)
              , basic $ Diagonal ( 0.5 * topWidth, 10) (-0.5 * botWidth, 0)
              ]
        }
    , let width = 7
      in Spec {
          specLetter  = 'Y'
        , specOffset  = 5
        , specStrokes =
            mconcat [
                basic $ Diagonal (-0.5 * width, 10) (0, 5)
              , basic $ Diagonal ( 0.5 * width, 10) (0, 5)
              , basic $ Vertical (0, 5) 5
              ]
        }
    , let topWidth = 7.7
          botWidth = 8
      in Spec {
          specLetter  = 'Z'
        , specOffset  = 9
        , specStrokes =
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

