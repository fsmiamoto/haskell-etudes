module NinetyNine where

-- Problem 1
myLast :: [a] -> a
myLast [] = error "no last element for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' = head . reverse

-- Problem 2
myButLast :: [a] -> a
myButLast [] =  error "No last but one element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

myButLast' = last . init

myButLast'' = head . tail . reverse

-- Problem 3

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) index = elementAt xs (index-1)
