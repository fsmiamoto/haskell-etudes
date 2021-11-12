module Person where

import Prelude hiding (Either(..) )

type Name = String
type Age = Int

data Person = Person Name Age
    deriving (Show)

data Either a b = Left  a
                | Right b
    deriving (Show, Eq)

data PersonError = TooOld | NameTooShort
    deriving (Show, Eq)

mkPerson :: Name -> Age -> Either PersonError Person
mkPerson name age
    | age > 200 = Left TooOld
    | length name < 2 = Left NameTooShort
    | otherwise = Right $ Person name age
