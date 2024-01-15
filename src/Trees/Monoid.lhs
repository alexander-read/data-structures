> {-# OPTIONS_GHC -Wall -Werror #-}

> module Trees.Monoid ( treeSum,
>                       treeProduct,
>                       treeAnd,
>                       treeOr,
>                       treeAll,
>                       treeAny,
>                       Add(..),
>                       Mul(..),
>                       And(..),
>                       Or(..),
>                     ) where

We define some useful functions for folding over trees.

{--------------------------------------------------------------------------}
{-- Functions on Trees --}

> -- | Smullyan's blackbird combinator (cf. Amar Shah)
> (...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
> (...) = (.) . (.) -- \ f g x y . f (g x y)

Functions for our trees, but we may as well make them work for any foldable:

> treeSum :: Foldable t => t Int -> Int
> treeSum = getAdd . foldMap Add

> treeProduct :: Foldable t => t Int -> Int
> treeProduct = getMul . foldMap Mul

> treeAnd :: Foldable t => t Bool -> Bool
> treeAnd = getAnd . foldMap And

> treeOr :: Foldable t => t Bool -> Bool
> treeOr = getOr . foldMap Or

> treeAll :: Foldable t => (a -> Bool) -> t a -> Bool
> treeAll p = getAnd ... foldMap $ And . p

> treeAny :: Foldable t => (a -> Bool) -> t a -> Bool
> treeAny p = getOr ... foldMap $ Or . p

The definitions for `treeAll` and `treeAny` could be point-free, for example
we would define the former as `getAnd ... (foldMap . (And .))`:

    (And .) :: (a -> Bool) -> a -> And
    foldMap :: Monoid m    => (a -> m) -> t a -> m

Note that `(And .)` takes a predicate and lifts it into a function that returns
a monoid value. That function can be given as the first argument to `foldMap`.
So, their composition is of type (a -> Bool) -> t a -> m.

{--------------------------------------------------------------------------}
{-- Type Class Instances --}

> -- | Wrappers for integers
> newtype Add = Add { getAdd :: Int  } deriving (Eq, Show)
> newtype Mul = Mul { getMul :: Int  } deriving (Eq, Show)

> -- | Wrappers for booleans
> newtype And = And { getAnd :: Bool } deriving (Eq, Show)
> newtype Or  = Or  { getOr  :: Bool } deriving (Eq, Show)

{---- Semigroup ----}

Associative operations for integers and booleans:

> instance Semigroup Mul where
>   n <> m = Mul $ (getMul n) * (getMul m)

> instance Semigroup Add where
>   n <> m = Add $ (getAdd n) + (getAdd m)

> instance Semigroup And where
>   p <> q = And $ (getAnd p) && (getAnd q)

> instance Semigroup Or where
>   p <> q = Or $ (getOr p) || (getOr q)

{---- Monoid ----}

Identities for integers and booleans:

> instance Monoid Mul where
>   mempty = Mul 1

> instance Monoid Add where
>   mempty = Add 0

> instance Monoid And where
>   mempty = And True

> instance Monoid Or where
>   mempty = Or False
