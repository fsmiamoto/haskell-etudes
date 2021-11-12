module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

import Prelude hiding (gcd)

type Numerator = Integer
type Denominator = Integer

data Rat = Rat Numerator Denominator

instance Show Rat where
    show (Rat n d)= show n ++ "/" ++ show d

instance Eq Rat where
    (==) (Rat i j) (Rat k l) = (i*l) == (j*k)

instance Num Rat where
    (+) (Rat i j) (Rat k l) = rat (i*l+k*j) (j*l)

    (*) (Rat i j) (Rat k l) = rat (i*k) (j*l)

    negate (Rat i j) = Rat (-i) j

    -- ASSUMPTION j > 0
    abs (Rat i j) = rat x j where x = abs i
    signum (Rat i j) = rat (signum i) 1

    fromInteger x = rat x 1

instance Ord Rat where
    compare (Rat i j) (Rat k l) = compare (i*l) (j*k) 

rat :: Integer -> Integer -> Rat
rat x y 
    | y == 0    = error "Denominator cannot be zero"
    | otherwise = Rat (x `div` d) (y `div` d) where d = gcd x y

(//) :: Rat -> Rat -> Rat
(//) (Rat i j) (Rat k l)= rat (i*l) (j*k)

denominator :: Rat -> Integer
denominator (Rat _ d) = d 

numerator :: Rat -> Integer
numerator (Rat n _) = n

gcd :: Integer -> Integer -> Integer
gcd 0 x = x
gcd x 0 = x
gcd x y
    | x < y     = gcd x (y `rem` x)
    | otherwise = gcd (x `rem` y) y
