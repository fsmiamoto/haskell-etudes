module Map where

import Prelude hiding ( map )

map :: (a -> b) -> [a] -> [b]
map f xs =   [ f x | x <- xs]
