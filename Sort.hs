module Sort 
    ( sort
    , msort
    , qsort
    , isort
    ) where

sort :: Ord a => [a] -> [a]
sort = msort

qsort :: Ord a => [a] -> [a]
qsort = undefined

msort :: Ord a => [a] -> [a]
msort [] = []
msort [z] = [z]
msort zs = merge (msort xs) (msort ys)
    where
        (xs,ys) = splitAt midpoint zs
        midpoint = length zs `div` 2

merge' :: Ord a => [a] -> [a] -> [a]
merge' xs ys = mergeiter xs ys []

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x:xs) ys'@(y:ys)
    | x <= y = x : merge xs ys'
    | otherwise = y : merge xs' ys

-- Too imperative
mergeiter :: Ord a => [a] -> [a] -> [a] -> [a]
mergeiter [] ys result = result ++ ys
mergeiter xs [] result = result ++ xs
mergeiter (x:xs) (y:ys) result = 
    if x < y then 
        mergeiter xs (y:ys) (result ++ [x]) 
    else 
        mergeiter (x:xs) ys (result ++ [y])

isort :: Ord a => [a] -> [a]
isort = undefined
