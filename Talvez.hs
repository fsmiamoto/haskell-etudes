module Talvez where

import Prelude hiding ( Maybe(..) )

data Maybe a = Nothing | Just a deriving (Show, Eq)

firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat _ []     = Nothing
firstThat p (x:xs) =  if p x then Just x else firstThat p xs

isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat isgood p xs =
    case firstThat p xs of
        Just x -> Just $ isgood x
        _ -> Nothing
