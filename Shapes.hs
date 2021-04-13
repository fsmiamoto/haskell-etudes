module Shapes where

data Color = Red | Green | Blue deriving (Show,Eq)

data Shape = Circle Double
           | Square Double 
           | Rectangle Double Double
           deriving (Show,Eq)


-- The problem of overusing aliases is that they can overlap and in those cases
-- Haskell won't be able to prevent us of making a type mistake
-- Take for example the following:
-- type Point = (Double,Double,Double)
-- type Color = (Double,Double,Double)
-- If you pass a Point where you expected a Color, things might go wrong.

data RGBColor = RGBColor Double Double Double
data Point3D = Point3D Double Double Double

data Pair a b = Pair a b deriving (Show,Eq)

outl :: Pair a b -> a
outl (Pair a _) = a

outr :: Pair a b -> b
outr (Pair _ b) = b

area :: Shape -> Double
area (Circle r) = pi  * r * r
area (Square s) = s * s
area (Rectangle w h ) = w * h

rot90 :: Shape -> Shape
rot90 (Rectangle w h ) = Rectangle h w
rot90 s                = s

height :: Shape -> Double
height (Circle r)      = 2 * r
height (Square s)    = 2* s
height (Rectangle _ h) = h
