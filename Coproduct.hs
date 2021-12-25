module Coproduct where

import Prelude hiding (Either(..))

data Either a b = Left a 
                | Right b
                deriving (Show, Eq)

f :: a -> d
f = undefined

g :: b -> d
g = undefined

h :: Either a b -> d
h (Left a)  = f a
h (Right b) = g b
