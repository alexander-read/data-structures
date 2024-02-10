> {-# OPTIONS_GHC -Wall               #-}
> {-# OPTIONS_GHC -Werror             #-}
> {-# OPTIONS_GHC -Wno-name-shadowing #-}

> ----------------------------------------------------------------------------
> -- |
> -- Module      : Trees.FiniteMap
> -- Description : An implementation of a Finite Map using Binary Search Trees
> --
> -- TODO: add improvements from the exercise code
> --
> ----------------------------------------------------------------------------
> module Trees.FiniteMap
>   ( UnbalancedMap
>   , empty'
>   , update
>   , lookup
>   ) where

> import Prelude hiding ( lookup )

> import Trees.BinaryTree ( Tree(..) )

{------------------------------------------------------------------------}
{-- Implementing Finite Maps --}

> -- | See Okasaki (1998: 16)
> class FiniteMap t k v where
>   empty' :: Ord k => t k v
>   update :: Ord k => k -> v -> t k v -> t k v
>   lookup :: Ord k => k -> t k v -> Maybe v

A finite map can be represented with a binary search tree.
This map is 'unbalanced' because the tree is not self-balancing.

> -- | Unwrap the (key, value) types for the typeclass instance.
> newtype UnbalancedMap k v = M (Tree (k, v))

The `Ord` instance on keys means that the values can be ordered symmetrically.

> instance Ord k => FiniteMap UnbalancedMap k v where
>   empty' = M Leaf
>
>   update k x (M tree) = M $ mapInsert k x tree
>     where
>       mapInsert k x Leaf                  = T Leaf (k, x) Leaf
>       mapInsert k x (T l y r) | k < fst y = T (mapInsert k x l) y r
>                               | k > fst y = T l y (mapInsert k x r)
>                               | otherwise = T l y r
>
>   lookup _ (M Leaf)                   = Nothing
>   lookup k (M (T l y r )) | k < fst y = lookup k $ M l
>                           | k > fst y = lookup k $ M r
>                           | otherwise = Just $ snd y

The `update` function uses an auxiliary `mapInsert` on trees.
This is because each subtree in an `UnbalancedMap` won't be wrapped with
an `M` constructor, so we need an insert function on trees for recursion.
However, `update` needs to return an `UnbalancedMap`, so the result of
`mapInsert` is wrapped with an `M` constructor.

The 'persistent' behaviour of `mapInsert` is analogous to `Set.insert`.
