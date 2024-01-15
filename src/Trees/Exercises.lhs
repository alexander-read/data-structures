> {-# OPTIONS_GHC -Wall -Werror #-}
> {-# OPTIONS_GHC -Wno-name-shadowing #-}

> module Trees.Exercises where

> import Trees.BinaryTree ( Tree(..) )

{------------------------------------------------------------------------}
{-- Exercise 2.2 (Okasaki, 1998) --}

The original `member` function performs `2d` comparisons in the worst case
where `d` is the depth of the tree. Re-write `member` so that it takes no more
than d+1 comparisons.

> member' :: Ord a => a -> Tree a -> Bool
> member' x t = worker [] x t
>   where
>     worker el x Leaf      = x `elem` el
>     worker el x (T l y r) = case x < y of
>         True  -> worker el x l
>         False -> worker [y] x r

This performs one comparison per node, and an equality check at the bottom.

Note: Okasaki mentions 'depth' of the tree, but I'm familiar with that only as
a property of nodes, so I assume him to mean 'length of longest path', namely
the height of the tree. Using (e.g.) the old `member` function, `member 4 tree`
performs seven comparisons, while with the new function, it performs four
including the final `elem` check. The height of this tree is three:

> tree :: Tree Int
> tree = T (T (T Leaf 1 Leaf) 2 Leaf) 3
>          (T (T Leaf 4 Leaf) 8 Leaf)

{------------------------------------------------------------------------}
{-- Exercise 2.3 (Okasaki, 1998) --}

Inserting an existing element into a binary search tree copies the entire
search path even though the copied nodes are indistinguishable from the originals.
Rewrite insert using exceptions to avoid this copying.

> insert' :: Ord a => a -> Tree a -> Tree a
> insert' x t = maybe t id (exists x t)
>   where
>     exists :: Ord a => a -> Tree a -> Maybe (Tree a)
>     exists x Leaf                  = return (T Leaf x Leaf)
>     exists x (T l y r) | x < y     = exists x l >>= \l' -> return (T l' y r)
>                        | x > y     = exists x r >>= \r' -> return (T l y r')
>                        | otherwise = Nothing

We only copy the whole search path and return a new tree if the element to be
inserted does not already exist in the tree. Otherwise, return the input tree.

{--------------------------------------------------------------------------}
{-- Exercise 2.4 (Okasaki, 1998) --}

Combine the ideas of the previous two exercises to obtain a version of insert
that performs no unnecessary copying and uses no more than d + 1 comparisons.

> insert'' :: Ord a => a -> Tree a -> Tree a
> insert'' x t = maybe t id (exists [] x t)
>   where
>     exists acc x Leaf      = if x `elem` acc then Nothing else return (T Leaf x Leaf)
>     exists acc x (T l y r) = case x < y of
>                                True  -> exists acc x l >>= \l' -> return (T l' y r)
>                                False -> exists [y] x r >>= \r' -> return (T l y r')

{--------------------------------------------------------------------------}
{-- Exercise 2.5 (Okasaki, 1998) --}

Suppose two subtrees of a given node are identical.
Then, they can be represented by the same tree.

Part (a): write a function `complete x d` that creates a complete binary tree
of depth `d` with `x` stored in every node. This should run in O(d) time.

> complete :: a -> Int -> Tree a
> complete x d | d < 0     = Leaf
>              | otherwise = T subtree x subtree
>   where
>     subtree = complete x $ d - 1

Each subtree is the same tree, and each of their subtrees are the same tree, etc.
This is done by putting `subtree` into a `where` clause.

Part (b): extend `complete` to create balanced trees of arbitrary size.
These trees will not always be complete but should be as balanced as possible.
This function should run in O(log n) time.

> complete' :: a -> Int -> Tree a
> complete' x n | n == 0    = Leaf
>               | n == 1    = T Leaf x Leaf
>               | even n    = T l x r
>               | otherwise = T l x l
>   where
>     (l, r) = create2 ((n - 1) `div` 2) x

> create2 :: Int -> a -> (Tree a, Tree a)
> create2 m x | m < 0     = (Leaf, Leaf)
>             | m == 0    = (Leaf, T Leaf x Leaf)
>             | even m    = ((T l x r), (T r x r))
>             | otherwise = ((T h x h), (T h x k))
>   where
>     (l, r) = create2 ((m - 2) `div` 2) x
>     (h, k) = create2 ((m - 1) `div` 2) x

The `create2` function creates a pair of trees, of sizes m and m + 1.

{--------------------------------------------------------------------------}
{-- Exercise 2.6 (Okasaki, 2008) --}

See `FiniteMap.lhs`.
