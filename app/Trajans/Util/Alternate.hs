-- | Lists of alternating types
--
-- Intended for qualified import.
--
-- > import Trajans.Util.Alternate (Alternate)
-- > import Trajans.Util.Alternate qualified as Alt
module Trajans.Util.Alternate (
    Alternate(..)
    -- * Construction
  , fromList
    -- * Combinators
  , head
  , append
  , concat
  , traverse
  , partition
  ) where

import Prelude hiding (concat, traverse, head)

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..), (<|))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Alternate between @a@s and @b@s, starting and ending on a @a@
data Alternate a b =
    AltDone a
  | AltCons a b (Alternate a b)
  deriving (Show)

instance Bifunctor Alternate where
  bimap :: forall a a' b b'.
       (a -> a')
    -> (b -> b')
    -> Alternate a b -> Alternate a' b'
  bimap f g = go
    where
      go :: Alternate a b -> Alternate a' b'
      go (AltDone a)      = AltDone (f a)
      go (AltCons a b xs) = AltCons (f a) (g b) (go xs)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

fromList :: forall a b. (a -> a -> b) -> NonEmpty a -> Alternate a b
fromList f (x :| xs) = go x xs
  where
    go :: a -> [a] -> Alternate a b
    go a []      = AltDone a
    go a (a':as) = AltCons a (f a a') (go a' as)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

append :: forall a b.
     (a -> a -> b)
  -> Alternate a b -> Alternate a b -> Alternate a b
append f = go
  where
    go :: Alternate a b -> Alternate a b -> Alternate a b
    go (AltDone a)         (AltDone a')     = AltCons a (f a a') (AltDone a')
    go (AltDone a)      ys@(AltCons a' _ _) = AltCons a (f a a') ys
    go (AltCons a b xs) ys                  = AltCons a b (go xs ys)

concat :: (a -> a -> b) -> NonEmpty (Alternate a b) -> Alternate a b
concat = foldr1 . append

traverse :: forall f a a' b b'.
     Applicative f
  => (a -> f a')
  -> (a -> b -> a -> f b')
  -> Alternate a b
  -> f (Alternate a' b')
traverse f g = go
  where
    go :: Alternate a b -> f (Alternate a' b')
    go (AltDone a)                       = one  <$> f a
    go (AltCons a b (AltDone a'))        = two  <$> f a <*> g a b a' <*> f a'
    go (AltCons a b xs@(AltCons a' _ _)) = more <$> f a <*> g a b a' <*> go xs

partition :: Alternate a b -> (NonEmpty a, [b])
partition (AltDone a)      = (a :| [], [])
partition (AltCons a b xs) = bimap (a <|) (b:) (partition xs)

head :: Alternate a b -> a
head (AltDone a)     = a
head (AltCons a _ _) = a

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

one :: a -> Alternate a b
one = AltDone

two :: a -> b -> a -> Alternate a b
two a b a' = AltCons a b (AltDone a')

more :: a -> b -> Alternate a b -> Alternate a b
more = AltCons