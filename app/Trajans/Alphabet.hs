module Trajans.Alphabet (
    Alphabet
  , mkAlphabet
  ) where

import Data.Map (Map)
import Data.Map qualified as Map

import Trajans.Letter

type Alphabet = Char -> Letter

mkAlphabet :: [Letter] -> Alphabet
mkAlphabet = findLetter . Map.fromList . map aux
  where
    aux :: Letter -> (Char, Letter)
    aux l = (letterName l, l)

    findLetter :: Map Char Letter -> Char -> Letter
    findLetter m c = case Map.lookup c m of
                       Just l  -> l
                       Nothing -> error $ "Unknown letter: " ++ show c

