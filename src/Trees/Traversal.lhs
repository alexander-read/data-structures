> {-# OPTIONS_GHC -Wall               #-}
> {-# OPTIONS_GHC -Werror             #-}
> {-# OPTIONS_GHC -Wno-name-shadowing #-}

> ----------------------------------------------------------------------------
> -- |
> -- Module      : Trees.Traversal
> -- Description : Useful functions for traversing binary trees
> --
> -- TODO: implement FIFO queues from Okasaki (1998: 42), then write functions
> -- for breadth-first traversal (cf. Okasaki, 2000).
> --
> ----------------------------------------------------------------------------
> module Trees.Traversal
>   ( preord
>   , inord
>   , postord
>   ) where

> import Trees.BinaryTree ( Tree(..) )

{------------------------------------------------------------------------}
{-- Depth-First Traversal --}

Depth-first traversals on binary trees. Note that `inord` on a binary search
tree will give you a sorted list of the elements, given the symmetry invariant
(cf. treesort algorithm).

> -- | Pre-order traversal of a binary tree
> preord :: Tree a -> [a]
> preord tree = worker tree []
>   where
>     worker Leaf ys      = ys
>     worker (T l x r) ys = x : (worker l (worker r ys))

> -- | In-order traversal of a binary tree
> inord :: Tree a -> [a]
> inord tree = worker tree []
>   where
>     worker Leaf ys      = ys
>     worker (T l x r) ys = worker l (x : worker r ys)

> -- | Post-order traversal of a binary tree
> postord :: Tree a -> [a]
> postord tree = worker tree []
>   where
>     worker Leaf ys      = ys
>     worker (T l x r) ys = worker l (worker r (x : ys))

{--------------------------------------------------------------------------}
{- Discussion -}

Here's a naive implementation of (e.g.) a pre-order traversal:

  preorder :: Tree a -> [a]
  preorder Leaf      = []
  preorder (T l x r) = [x] ++ preorder l ++ preorder r

However, the complexity is O(n^2), i.e., quadratic, because of append.
To improve performance we use the worker-wrapper transformation (cf. Hutton & Gill).
For the faster pre-order traversal function, `preord`, we define it as a wrapper
for a worker function:

  preord :: Tree a -> [a]
  preord tree = worker tree []

The worker is then specified as:

  worker :: Tree a -> [a] -> [a]
  worker tree ys = preorder tree ++ ys

Case for Leaf:

      worker Leaf xs
    = preorder Leaf ++ xs {specification of worker}
    = [] ++ xs            {definition of preorder}
    = xs                  {definition of append}

Case for (T l x r):

      worker (T l x r) xs
    = preorder (T l x r) ++ xs                    {specification of worker}
    = (([x] ++ preorder l) ++ preorder r) ++ xs   {definition of preorder}
    = ([x] ++ (preorder l ++ preorder r)) ++ xs   {associativity of append}
    = [x] ++ ((preorder l ++ preorder r) ++ xs)   {associativity of append}
    = x : ((preorder l ++ preorder r) ++ xs)      {definition of append}
    = x : (preorder l ++ (preorder r ++ xs))      {associativity of append}
    = x : (preorder l ++ (worker r xs))           {specification of worker}
    = x : (worker l (worker r xs))                {specification of worker}

We can do the same thing for in-order and post-order traversals.
