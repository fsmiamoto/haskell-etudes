module List where

import qualified Data.List as L

data Empty

data ListInt = Empty | ListInt Int ListInt

instance Show ListInt where
  show Empty = "]"
  show (ListInt h t) = "[" ++ show h ++ "," ++ show t

hd :: ListInt -> Int
hd Empty = error "can't get head of empty list"
hd (ListInt h _) = h

tl :: ListInt -> ListInt
tl Empty = error "can't get tail of empty list"
tl (ListInt _ t) = t

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = xs' ++ [x] where xs' = List.reverse xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

subsequences :: [a] -> [[a]]
subsequences (x : xs) =
  map (x :) xs' ++ xs'
  where
    xs' = subsequences xs
