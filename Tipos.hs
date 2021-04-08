module Tipos where

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance (Show Weekday) where
    show Mon = "月"
    show Tue = "火"
    show Wed = "水"
    show Thu = "木"
    show Fri = "金"
    show Sat = "土"
    show Sun = "日"

instance (Eq Weekday) where
    Mon == Mon = True 
    Tue == Tue = True
    Wed == Wed = True
    Thu == Thu = True
    Fri == Fri = True
    Sat == Sat = True
    Sun == Sun = True

nextDay :: Weekday -> Weekday
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

nextWorkingDay' :: Weekday -> Weekday
nextWorkingDay' Mon = Tue
nextWorkingDay' Tue = Wed
nextWorkingDay' Wed = Thu
nextWorkingDay' Thu = Fri
nextWorkingDay' _   = Mon

nextWorkingDay :: Weekday -> Weekday
nextWorkingDay Fri = Mon
nextWorkingDay Sat = Mon
nextWorkingDay Sun = Mon
nextWorkingDay x   = nextDay x

-- Steps for nextWorkingDay' (nextDay (nextWorkingDay Mon))
-- 1. Type check
-- nWd' :: W -> W, nD :: W -> W, nWd :: W -> W, nWd Mon, Mon :: W
--
-- 2. Substitute, starting from the left.
-- In this case, the first case for nextWorkingDay' matches Mon.
-- Haskell can only go to the next line if it guarantees that the argument
-- for nextWorkingDay' is not Mon, thus it has to make the computation.
-- So even though Haskell is lazy, in this case it has to make the computation 
-- beforehand
--
-- nextWorkingDay' (nextDay (Tue))
-- nextWorkingDay' (Wed)
-- Thu
--

-- Using the $ operator, we could write
-- nextWorkingDay' $ nextDay $ nextWorkingDay Mon
