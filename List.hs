module List where

data Pair a b = Pair a b

data Empty

data ListInt = Empty | ListInt Int ListInt deriving (Show,Eq)

hd :: ListInt -> Int
hd Empty = error "can't get head of empty list"
hd (ListInt h _)= h

tl :: ListInt -> ListInt
tl Empty = error "can't get tail of empty list"
tl (ListInt _ t) = t
