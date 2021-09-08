module Nats where

import Data.Char ( toUpper )

data Nat = Zero | Succ Nat 

plus :: Nat -> Nat -> Nat
plus n Zero = n
plus n (Succ m) = plus (Succ n) m

instance (Show Nat) where 
    show Zero = "O"
    show (Succ n) = 'S' : show n

len :: [a] -> Nat
len [] = Zero
len (_:t) = Succ (len t)


bottom :: a
bottom = bottom

atLeastTwo :: Nat
atLeastTwo = Succ (Succ bottom)

atleast :: Int -> Nat
atleast 0 = bottom
atleast n = Succ ( atleast (n - 1))
