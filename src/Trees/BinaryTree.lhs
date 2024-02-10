> {-# OPTIONS_GHC -Wall               #-}
> {-# OPTIONS_GHC -Werror             #-}
> {-# OPTIONS_GHC -Wno-name-shadowing #-}

> ----------------------------------------------------------------------------
> -- |
> -- Module      : Trees.BinaryTree
> -- Description : The Binary Tree Data Structure
> --
> ----------------------------------------------------------------------------
> module Trees.BinaryTree
>   ( Tree(..)
>   , sizeTree
>   , heightTree
>   , leavesTree
>   , depthTree
>   ) where

> import qualified Data.Monoid as M

{------------------------------------------------------------------------}
{-- Binary Trees --}

> -- | Binary tree with values stored only in nodes, not leaves.
> data Tree a = Leaf
>             | T (Tree a) a (Tree a)
>             deriving Show

A binary search tree is a binary tree whose elements are ordered
symmetrically. For any node (T l x r), `x` is greater than any
elements in `l`, and smaller than any elements in `r` (Okasaki: 11).

If the tree is balanced, most operations take O(log n) for a tree of size n.

{--------------------------------------------------------------------------}
{-- Type Class Instances --}

> instance Functor Tree where
>   fmap _ Leaf      = Leaf
>   fmap f (T l x r) = T (fmap f l) (f x) (fmap f r)

> instance Foldable Tree where
>   foldMap _ Leaf      = mempty
>   foldMap m (T l x r) = m x <> foldMap m l <> foldMap m r

> instance Traversable Tree where
>   traverse _ Leaf      = pure Leaf
>   traverse f (T l x r) = T <$> traverse f l <*> f x <*> traverse f r

{--------------------------------------------------------------------------}
{-- Utility Functions --}

> -- | Count the number of branches
> sizeTree :: Tree a -> Int
> sizeTree = M.getAdd . foldMap (const (M.Add 1))

> -- | Length of longest path
> heightTree :: Tree a -> Int
> heightTree Leaf      = 0
> heightTree (T l _ r) = 1 + max (heightTree l) (heightTree r)

> -- | Invariant: leaves t = (sizeTree t) + 1
> leavesTree :: Tree a -> Int
> leavesTree = (+ 1) . sizeTree

> -- | Depth of a given node
> depthTree :: Ord a => a -> Tree a -> Maybe Int
> depthTree v tree = go 0 v tree
>   where
>     go _ _ Leaf                  = Nothing
>     go n v (T l x r) | v < x     = go (n + 1) v l
>                      | v > x     = go (n + 1) v r
>                      | otherwise = Just n
