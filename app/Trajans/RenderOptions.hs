module Trajans.RenderOptions (
    Alignment(..)
  , Thickness(..)
  , Spacing(..)
  , RenderOptions(..)
    -- * Interpretation
  , intThickness
    -- * Parsing
  , parseRenderOptions
  ) where

import Data.Colour
import Data.Colour.Names
import Data.Foldable (asum)
import Data.Maybe
import Diagrams (Measure, thin, medium, thick)
import Options.Applicative

data Alignment =
    AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Show)

data Thickness =
    Thin
  | Medium
  | Thick
  deriving (Show)

data Spacing =
    -- | Proper letter spacing
    --
    -- The 'Bool' indicates if we want the spaces to be highlighted.
    Proper Bool

    -- | Regular letter spacing (grid structure)
  | Regular
  deriving (Show)

data RenderOptions = RenderOptions {
      renderAlignment :: Alignment
    , renderGrid      :: Bool
    , renderRulers    :: Bool
    , renderSpacing   :: Spacing
    , renderXCompr    :: Bool -- ^ Compress letters?
    , renderBounds    :: Bool -- ^ Show letter bounds?
    , renderThickness :: Thickness
    , renderColour    :: Colour Double
    , renderSlope     :: Double
    , renderDebug     :: Bool -- ^ For debugging the renderer
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Interpreter for some of the options
-------------------------------------------------------------------------------}

intThickness :: Thickness -> Measure Double
intThickness Thin   = thin
intThickness Medium = medium
intThickness Thick  = thick

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseRenderOptions :: Parser RenderOptions
parseRenderOptions =
    RenderOptions
      <$> (optional' AlignCenter parseAlignment)
      <*> (switch $ mconcat [
               long "grid"
             , help "Render a grid"
             ])
      <*> (switch $ mconcat [
               long "rulers"
             , help "Render line rulers"
             ])
      <*> (optional' (Proper False) $ option (eitherReader parseSpacing) $ mconcat [
               long "spacing"
             , help "Letter spacing (proper, highlight, regular)"
             ])
      <*> (switch $ mconcat [
               long "xcompr"
             , help "Compress letter width"
             ])
      <*> (switch $ mconcat [
               long "bounds"
             , help "Show letter bounds"
             ])
      <*> (optional' Thin $ option (eitherReader parseThickness) $ mconcat [
               long "thickness"
             , help "Stroke thickness (thin, medium, thick)"
             ])
      <*> (optional' black $ option (readColourName =<< str) $ mconcat [
               long "colour"
             , help "Stroke colour"
             ])
      <*> (option auto $ mconcat [
               long "slope"
             , help "Slope of the letters"
             , value 0
             , metavar "DEG"
             , showDefault
             ])
      <*> (switch $ mconcat [
               long "debug"
             , help "Debug the rendering code"
             ])

parseSpacing :: String -> Either String Spacing
parseSpacing "proper"    = Right $ Proper False
parseSpacing "highlight" = Right $ Proper True
parseSpacing "regular"   = Right $ Regular
parseSpacing x           = Left $ "Cannot parse spacing " ++ show x

parseThickness :: String -> Either String Thickness
parseThickness "thin"   = Right Thin
parseThickness "medium" = Right Medium
parseThickness "thick"  = Right Thick
parseThickness x        = Left $ "Cannot parse thickness " ++ show x

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
  Auxiliary
-------------------------------------------------------------------------------}

optional' :: Alternative f => a -> f a -> f a
optional' def = fmap (fromMaybe def) . optional