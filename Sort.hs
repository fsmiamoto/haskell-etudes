module Sort
  ( sort,
    msort,
    qsort,
    isort,
  )
where

sort :: Ord a => [a] -> [a]
sort = msort

msort :: Ord a => [a] -> [a]
msort [] = []
msort [z] = [z]
msort zs = merge (msort xs) (msort ys)
  where
    (xs, ys) = halve zs

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve (x : y : xs) = (x : lxs, y : rxs) where (lxs, rxs) = halve xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' xs ys = mergeiter xs ys []

-- ASSUMPTION: xs and ys are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x : xs) ys'@(y : ys)
  | x <= y = x : merge xs ys'
  | otherwise = y : merge xs' ys

-- Too imperative
mergeiter :: Ord a => [a] -> [a] -> [a] -> [a]
mergeiter [] ys result = result ++ ys
mergeiter xs [] result = result ++ xs
mergeiter (x : xs) (y : ys) result =
  if x < y
    then mergeiter xs (y : ys) (result ++ [x])
    else mergeiter (x : xs) ys (result ++ [y])

isort :: Ord a => [a] -> [a]
isort xs = foldr insert [] xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x ys'@(y : ys)
  | x <= y = x : ys'
  | otherwise = y : insert x ys

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p : xs) = qsort small ++ [p] ++ qsort large
  where
    small = filter (<= p) xs
    large = filter (> p) xs

sorted :: Ord a => [a] -> Bool
sorted (x : x' : xs) = x <= x' && sorted (x' : xs)
sorted _ = True

sorted' :: Ord a => [a] -> Bool
sorted' [] = True
sorted' l@(x : xs) = and (zipWith (<=) l xs)

-- import Test.QuickCheck as QC
-- ghci> quickCheck prop_qsortLength

prop_qsortLength xs = length xs == length (qsort xs)

prop_qsortSorted xs = sorted $ qsort xs

prop_qsortQsort xs = qsort xs == (qsort . qsort) xs
