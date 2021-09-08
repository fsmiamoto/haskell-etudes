module Talvez where

import Prelude hiding ( Maybe(..) )

data Maybe a = Nothing | Just a deriving (Show, Eq)

firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat pred xs = safeHead $ filter pred xs

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat filter pred xs =
    case firstThat pred xs of
        Just value -> Just $ filter value
        _ -> Nothing
