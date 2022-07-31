module ExList where

import qualified Data.Char as C
import qualified Data.List as L
import Prelude
  ( Bool (..),
    Char,
    Double,
    Enum (..),
    Eq (..),
    Float,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    String,
    curry,
    error,
    not,
    otherwise,
    uncurry,
    undefined,
    ($),
    (&&),
    (.),
    (||),
  )
import qualified Prelude as P

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head [] = error "no head for empty list"
head (x : _) = x

tail :: [a] -> [a]
tail xs = if null xs then error "boom" else t where (_ : t) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (x : xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (_ : xs) = 1 + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = append x xs' where xs' = reverse xs

append :: a -> [a] -> [a]
append x [] = [x]
append x (y : ys) = y : append x ys

(++) :: [a] -> [a] -> [a]
x ++ [] = x
[] ++ y = y
x ++ (y : ys) = append y x ++ ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc v (x : xs) = x : snoc v xs

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
xs +++ [y] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "empty list"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: Integral a => a -> [b] -> [b]
take _ [] = []
take 0 _ = []
take n (x : xs) = x : take (n - 1) xs

drop :: Integral a => a -> [b] -> [b]
drop _ [] = []
drop 0 xs = xs
drop n (_ : xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile pred (x : xs) = if pred x then dropWhile pred xs else xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) =
  map (x :) xs' ++ xs'
  where
    xs' = subsequences xs

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
