module RecList where

(+++) :: [a] -> [a]-> [a]
[] +++ ys =  ys
xs +++ [] =  xs
[x] +++ xs = x:xs
(x:xs) +++ ys = [x] +++ (xs +++ ys)
