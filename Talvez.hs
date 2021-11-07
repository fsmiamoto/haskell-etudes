module Talvez where

import Prelude hiding ( Maybe(..) )

data Maybe a = Nothing | Just a deriving (Show, Eq)

firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat _ []     = Nothing
firstThat p (x:xs) =  if p x then Just x else firstThat p xs

maybeize :: (a -> b) -> Maybe a -> Maybe b
maybeize _ Nothing = Nothing
maybeize f (Just a) = Just $ f a

isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat isgood p = maybeize isgood . firstThat p
