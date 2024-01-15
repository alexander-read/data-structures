> {-# OPTIONS_GHC -Wall -Werror #-}
> {-# OPTIONS_GHC -Wno-unused-top-binds #-}

> module Trees.Traversal ( preord,
>                          inord,
>                          postord
>                        ) where

> import Trees.BinaryTree ( Tree(..) )

TODO: implement FIFO queues from Okasaki (1998: 42), then write functions
for breadth-first traversal (cf. Okasaki, 2000).

{------------------------------------------------------------------------}
{-- Depth-First Traversal --}

Here's a naive implementation of (e.g.) a pre-order traversal:

> preorder :: Tree a -> [a]
> preorder Leaf      = []
> preorder (T l x r) = [x] ++ preorder l ++ preorder r

However, the complexity is O(n^2), i.e., quadratic, because of append.
To improve performance we use the worker-wrapper transformation (cf. Hutton & Gill):

> preord :: Tree a -> [a]
> preord tree = worker tree []
>   where
>     worker Leaf ys      = ys
>     worker (T l x r) ys = x : (worker l (worker r ys))

We can do the same thing for in-order and post-order traversals.
Note that `inord` on a binary search tree will give you a sorted list of
the elements, given the symmetry invariant (cf. treesort algorithm):

> inord :: Tree a -> [a]
> inord tree = worker tree []
>   where
>     worker Leaf ys      = ys
>     worker (T l x r) ys = worker l (x : worker r ys)

> postord :: Tree a -> [a]
> postord tree = worker tree []
>   where
>     worker Leaf ys      = ys
>     worker (T l x r) ys = worker l (worker r (x : ys))

{---- Deriving a Worker and a Wrapper ----}

We want a faster pre-order traversal function, `preord`.
So, we define `preord` as a wrapper for a worker function:

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
