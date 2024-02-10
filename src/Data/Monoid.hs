{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Data.Monoid
-- Description : Some useful Monoid instances for Data.Fold
--
----------------------------------------------------------------------------
module Data.Monoid
  ( Add(..)
  , Mul(..)
  , And(..)
  , Or(..)
  ) where

-- | Wrappers for integers
newtype Add = Add { getAdd :: Int  } deriving (Eq, Show)
newtype Mul = Mul { getMul :: Int  } deriving (Eq, Show)

-- | Wrappers for booleans
newtype And = And { getAnd :: Bool } deriving (Eq, Show)
newtype Or  = Or  { getOr  :: Bool } deriving (Eq, Show)

{---- Semigroup ----}

-- Associative operations for integers and booleans:

instance Semigroup Mul where
    n <> m = Mul $ (getMul n) * (getMul m)

instance Semigroup Add where
    n <> m = Add $ (getAdd n) + (getAdd m)

instance Semigroup And where
    p <> q = And $ (getAnd p) && (getAnd q)

instance Semigroup Or where
    p <> q = Or $ (getOr p) || (getOr q)

{---- Monoid ----}

-- Identities for integers and booleans:

instance Monoid Mul where
    mempty = Mul 1

instance Monoid Add where
    mempty = Add 0

instance Monoid And where
    mempty = And True

instance Monoid Or where
    mempty = Or False
