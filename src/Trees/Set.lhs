> {-# OPTIONS_GHC -Wall   #-}
> {-# OPTIONS_GHC -Werror #-}

> ----------------------------------------------------------------------------
> -- |
> -- Module      : Trees.Set
> -- Description : An implementation of the Set ADT using Binary Search Trees
> --
> -- TODO: define `delete` function
> --
> ----------------------------------------------------------------------------
> module Trees.Set
>   ( empty
>   , member
>   , insert
>   ) where

> import Trees.BinaryTree ( Tree(..) )

{------------------------------------------------------------------------}
{-- Implementing Sets --}

> -- | See Okasaki (1998: 202)
> class Set s a where
>   empty  :: Ord a => s a
>   insert :: Ord a => a -> s a -> s a
>   member :: Ord a => a -> s a -> Bool

> instance Ord a => Set Tree a where
>     empty = Leaf
>
>     member _ Leaf                  = False
>     member x (T l y r) | x < y     = member x l
>                        | x > y     = member x r
>                        | otherwise = True
>
>     insert x Leaf                  = T Leaf x Leaf
>     insert x (T l y r) | x < y     = T (insert x l) y r
>                        | x > y     = T l y (insert x r)
>                        | otherwise = T l y r
