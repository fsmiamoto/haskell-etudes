-- Exercise from the class Programacao Funcional of Prof. Thanos
-- Source: https://tsouanas.org/teaching/fun/2019.2/
module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero = "O"
    show (Succ x) = "S" ++ show x

instance Eq Nat where

    Zero == Zero = True
    Zero == x = False
    x == Zero = False
    (Succ x) == (Succ y) = x == y

instance Ord Nat where

    Zero <= Zero = True
    Zero <= x = True
    x <= Zero = False
    (Succ x) <= (Succ y) = x <= y

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min Zero Zero = Zero
    min Zero (Succ _) = Zero
    min (Succ _) Zero = Zero
    min (Succ x) (Succ y) = Succ $ min x y

    max Zero Zero = Zero
    max Zero (Succ x) = Succ x
    max (Succ x) Zero = Succ x
    max (Succ x) (Succ y) = Succ (max x y)

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero = True
even (Succ Zero) = False
even (Succ x) = odd x && even (pred x)

odd :: Nat -> Bool
odd Zero = False
odd (Succ Zero) = True
odd (Succ x) = even x && odd (pred x)

-- addition
(<+>) :: Nat -> Nat -> Nat
Zero <+> x = x
x <+> Zero = x
(Succ x) <+> y = x <+> Succ y

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
Zero <-> Zero = Zero
Zero <-> x = Zero
x <-> Zero = x
x <-> (Succ y) = pred x <-> y

-- multiplication
(<*>) :: Nat -> Nat -> Nat
Zero <*> _ = Zero
_ <*> Zero = Zero
x <*> (Succ Zero) = x
(Succ Zero) <*> x = x
x <*> y = x <+> (x <*> pred y)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^> Zero = Succ Zero
Zero <^> _ = Zero
x <^> (Succ Zero) = x
x <^> y = x <*> (x <^> pred y)

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> Zero = error "cannot divide by zero"
Zero </> _ = 0
x </> (Succ Zero) = x
x </> y  
    | x < y = Zero
    | x == y = Succ Zero  -- Not strictly necessary but good optimization
    | x > y = Succ Zero <+> ((x<->y) </> y)

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> Zero = error "cannot divide by zero" 
Zero <%> _ = Zero
x <%> (Succ Zero) = Zero
x <%> y  
    | x < y = x
    | x == y = Zero
    | x > y = x<->y <%> y 

-- divides
(<|>) :: Nat -> Nat -> Bool
x <|> y = (x <%> y) == Zero

divides = (<|>)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff x y  
    | x >= y = x <-> y
    | x < y = y <-> x

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero = 1
factorial (Succ Zero) = Succ Zero
factorial x = x <*> factorial (pred x)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg (Succ x) = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ Zero = error "log of Zero"
lo Zero _ = error "log base zero"
lo x (Succ Zero) = Zero
lo x y
    | x > y = Zero
    | x == y = Succ Zero
    | x < y = Succ Zero + lo x (y </> x)

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat x = Succ $ toNat $ x - 1

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat (Succ x) = 1 + fromNat x

-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = error "Nat can only be positive"
        | otherwise = toNat x

