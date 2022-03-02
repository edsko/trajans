module Trajans.Alphabet (
    Alphabet
  , mkAlphabet
  ) where

import Data.Map qualified as Map

import Trajans.Letter

type Alphabet = Char -> Letter

mkAlphabet :: [Letter] -> Alphabet
mkAlphabet = (Map.!) . Map.fromList . map aux
  where
    aux :: Letter -> (Char, Letter)
    aux l = (letterName l, l)

