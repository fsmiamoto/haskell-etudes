module Boolean where

data Boolean  = F | T deriving (Show)

lor :: Boolean -> Boolean -> Boolean
lor F F = F
lor _ _ = T

land :: Boolean -> Boolean -> Boolean
land T T = T
land _ _ = F

lnot :: Boolean -> Boolean
lnot F = T
lnot _ = F

-- In Haskell, the if-then-else expression must return the same type
ifthenelse :: Boolean -> a -> a -> a
ifthenelse T a _  = a
ifthenelse _ _ b = b

instance (Eq Boolean) where
    T == T = True
    F == F = True
    _ ==  _ = False

