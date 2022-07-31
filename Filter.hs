module Filter where

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' pred xs = concatMap (\x -> [x | pred x]) xs
