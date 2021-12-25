module Either where

data Pair a b = Pair a b

Pair :: a -> b -> Pair a b

data Either a b = Left a | Right b

Left  :: a -> Either a b
Right :: b -> Either a b

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

const' :: a -> (b -> a)
const' x = \_ -> x
