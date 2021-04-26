module MyList where

data Empty

data List a = Empty | Cons a (List a) deriving (Show,Eq)

len :: [a] -> Int
len [] = 0
len (_ : t) = 1 + len t

-- len :: List a -> Int
-- len Empty = 0
-- len (Cons _ t) = 1 + len t

rev :: [a] -> [a]
rev [] = []
rev (x : xs)= xs' ++ [x] 
    where xs' = rev xs

atLeastTwo :: [a] -> Bool
atLeastTwo [] = False
atLeastTwo [x] = False
atLeastTwo _ = True

firstTwo :: [a] -> (a,a)
firstTwo xs 
    | not $ atLeastTwo xs = error "does not have at least two elements"
    | otherwise = (x, y)  where (x:y:_) = xs
