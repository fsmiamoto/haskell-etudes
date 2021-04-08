module Average where

av :: Fractional a => a -> a -> a
av x y = (x + y) / 2

av2 :: Fractional a => (a,a) -> a
av2 (x,y) = (x+y)/2
