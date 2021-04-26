module List where

data Empty

data ListInt = Empty | ListInt Int ListInt 

instance Show ListInt where
    show Empty = "]"
    show (ListInt h t) = "[" ++ show h ++ "," ++ show t

hd :: ListInt -> Int
hd Empty = error "can't get head of empty list"
hd (ListInt h _)= h

tl :: ListInt -> ListInt
tl Empty = error "can't get tail of empty list"
tl (ListInt _ t) = t
