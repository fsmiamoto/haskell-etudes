module Nats where

data Nat = Zero | Succ Nat 

plus :: Nat -> Nat -> Nat
plus n Zero = n
plus n (Succ m) = plus (Succ n) m

instance (Show Nat) where 
    show Zero = "O"
    show (Succ m) = "S" ++ show m

