Hello world!

Let's implement quicksort using haskell

> module Sort where

Starting with the module

> qsort :: Ord a => [a] -> [a]
> qsort [] = []
> qsort (p:xs) = qsort smaller ++ [p] ++ qsort larger
>   where
>       smaller = filter (<=p) xs
>       larger = filter  (>p)  xs

Let's test it out!
> qsort [1,4,2,52,4,6]
