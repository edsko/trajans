module Trajans.Alphabet (
    Alphabet
  , mkAlphabet
  ) where

import Data.Map qualified as Map

import Trajans.Letter

type Alphabet = Char -> Letter

mkAlphabet :: [Spec] -> Alphabet
mkAlphabet = (Map.!) . Map.fromList . map aux
  where
    aux :: Spec -> (Char, Letter)
    aux spec = (specLetter spec, mkLetter spec)

