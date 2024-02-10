{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Data.Fold
-- Description : Utility functions for foldable data structures
--
----------------------------------------------------------------------------
module Data.Fold
  ( foldSum
  , foldProduct
  , foldAnd
  , foldOr
  , foldAll
  , foldAny
  ) where

import Data.Monoid

-- | Smullyan's blackbird combinator (cf. Amar Shah)
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.) -- \ f g x y . f (g x y)

foldSum :: Foldable t => t Int -> Int
foldSum = getAdd . foldMap Add

foldProduct :: Foldable t => t Int -> Int
foldProduct = getMul . foldMap Mul

foldAnd :: Foldable t => t Bool -> Bool
foldAnd = getAnd . foldMap And

foldOr :: Foldable t => t Bool -> Bool
foldOr = getOr . foldMap Or

foldAll :: Foldable t => (a -> Bool) -> t a -> Bool
foldAll p = getAnd ... foldMap $ And . p

foldAny :: Foldable t => (a -> Bool) -> t a -> Bool
foldAny p = getOr ... foldMap $ Or . p

-- The definitions for `treeAll` and `treeAny` could be point-free, for example
-- we would define the former as `getAnd ... (foldMap . (And .))`:

--    (And .) :: (a -> Bool) -> a -> And
--    foldMap :: Monoid m    => (a -> m) -> t a -> m

-- Note that `(And .)` takes a predicate and lifts it into a function that returns
-- a monoid value. That function can be given as the first argument to `foldMap`.
-- So, their composition is of type (a -> Bool) -> t a -> m.
