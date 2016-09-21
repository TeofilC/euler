import Data.Time

start = fromGregorian 1901 1 6

day (_,_,a) = a
year (a,_,_) = a

f d | (year $ toGregorian d) <= 2000 = (if (day . toGregorian) d == 1 then 1 else 0) + f (addDays 7 d)
    | otherwise = 0


main = print $ f start
